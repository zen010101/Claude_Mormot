/// MCP stdio Transport Layer using TProcess
// - Cross-platform implementation using FPC's TProcess
// - Handles subprocess creation and stdin/stdout pipe communication
// - Message format: Content-Length header + JSON body
unit mcp.transport;

{$I mormot.defines.inc}

{$ifdef MSWINDOWS}
  {$APPTYPE CONSOLE}
{$endif}

interface

uses
  {$I mormot.uses.inc}
  Classes,
  SysUtils,
  Process,
  mormot.core.base,
  mormot.core.os,
  mormot.core.text,
  mormot.core.unicode,
  mormot.core.variants,
  mormot.core.json;

type
  /// Exception raised for MCP transport errors
  EMCPTransport = class(Exception);

  /// Event for receiving messages from the server
  TOnMCPMessage = procedure(const aMessage: RawUtf8) of object;

  /// Event for logging (standalone procedure, not object method)
  TOnMCPLog = procedure(const aMessage: string);

  /// stdio Transport for MCP protocol using TProcess
  // - Creates a subprocess and communicates via stdin/stdout pipes
  // - Uses Content-Length framing as per LSP/MCP specification
  TMCPStdioTransport = class
  private
    fProcess: TProcess;
    fReaderThread: TThread;
    fOnMessage: TOnMCPMessage;
    fOnLog: TOnMCPLog;
    fCommand: TFileName;
    fArguments: TStringList;
    fEnvironment: TStringList;
    fWorkingDir: TFileName;
    fRunning: boolean;
    fShuttingDown: boolean;
    procedure DoLog(const aMsg: string); overload;
    procedure DoLog(const aFmt: string; const aArgs: array of const); overload;
  protected
    procedure ReaderThreadExecute;
  public
    constructor Create;
    destructor Destroy; override;
    /// Start the subprocess and begin communication
    procedure Start;
    /// Stop the subprocess gracefully
    procedure Stop;
    /// Send a raw message to the server (adds Content-Length header)
    procedure SendMessage(const aContent: RawUtf8);
    /// Check if the transport is running
    property Running: boolean read fRunning;
    /// Command to execute (e.g., 'uv', 'python')
    property Command: TFileName read fCommand write fCommand;
    /// Command arguments
    property Arguments: TStringList read fArguments;
    /// Environment variables (NAME=VALUE format)
    property Environment: TStringList read fEnvironment;
    /// Working directory for the subprocess
    property WorkingDir: TFileName read fWorkingDir write fWorkingDir;
    /// Called when a message is received from the server
    property OnMessage: TOnMCPMessage read fOnMessage write fOnMessage;
    /// Called for logging
    property OnLog: TOnMCPLog read fOnLog write fOnLog;
  end;

  /// Reader thread for async stdout reading
  TMCPReaderThread = class(TThread)
  private
    fTransport: TMCPStdioTransport;
  protected
    procedure Execute; override;
  public
    constructor Create(aTransport: TMCPStdioTransport);
  end;

implementation

{ TMCPStdioTransport }

constructor TMCPStdioTransport.Create;
begin
  inherited Create;
  fArguments := TStringList.Create;
  fEnvironment := TStringList.Create;
  fRunning := false;
  fShuttingDown := false;
end;

destructor TMCPStdioTransport.Destroy;
begin
  Stop;
  fArguments.Free;
  fEnvironment.Free;
  inherited Destroy;
end;

procedure TMCPStdioTransport.DoLog(const aMsg: string);
begin
  if Assigned(fOnLog) then
    fOnLog(aMsg);
end;

procedure TMCPStdioTransport.DoLog(const aFmt: string; const aArgs: array of const);
begin
  DoLog(Format(aFmt, aArgs));
end;

procedure TMCPStdioTransport.Start;
var
  i: integer;
begin
  if fRunning then
    Exit;

  if fCommand = '' then
    raise EMCPTransport.Create('Command not specified');

  DoLog('Starting MCP server: %s', [fCommand]);

  fProcess := TProcess.Create(nil);
  try
    fProcess.Executable := fCommand;

    // Add arguments
    for i := 0 to fArguments.Count - 1 do
      fProcess.Parameters.Add(fArguments[i]);

    // Add environment variables
    // First copy current environment
    for i := 0 to GetEnvironmentVariableCount - 1 do
      fProcess.Environment.Add(GetEnvironmentString(i));
    // Then add custom ones
    for i := 0 to fEnvironment.Count - 1 do
      fProcess.Environment.Add(fEnvironment[i]);

    if fWorkingDir <> '' then
      fProcess.CurrentDirectory := fWorkingDir;

    // Enable pipes for stdin/stdout (do NOT merge stderr - it breaks Content-Length parsing)
    fProcess.Options := [poUsePipes];

    DoLog('Executing: %s %s', [fCommand, fArguments.CommaText]);
    fProcess.Execute;

    if not fProcess.Running then
      raise EMCPTransport.Create('Failed to start process');

    fRunning := true;
    fShuttingDown := false;

    // Start reader thread
    fReaderThread := TMCPReaderThread.Create(Self);

    DoLog('MCP server started, PID: %d', [fProcess.ProcessID]);
  except
    on E: Exception do
    begin
      FreeAndNil(fProcess);
      raise EMCPTransport.CreateFmt('Failed to start MCP server: %s', [E.Message]);
    end;
  end;
end;

procedure TMCPStdioTransport.Stop;
begin
  if not fRunning then
    Exit;

  DoLog('Stopping MCP server...');
  fShuttingDown := true;

  // FIRST: Terminate the process to unblock the reader thread's blocking Read()
  if Assigned(fProcess) then
  begin
    if fProcess.Running then
    begin
      // Close input first (graceful shutdown signal)
      fProcess.CloseInput;
      Sleep(50);
      // Then terminate - this unblocks the reader thread's Read()
      if fProcess.Running then
        fProcess.Terminate(0);
    end;
  end;

  // THEN: Wait for reader thread (now unblocked because process is terminated)
  if Assigned(fReaderThread) then
  begin
    fReaderThread.Terminate;
    fReaderThread.WaitFor;
    FreeAndNil(fReaderThread);
  end;

  // Finally: Free the process
  FreeAndNil(fProcess);

  fRunning := false;
  DoLog('MCP server stopped');
end;

procedure TMCPStdioTransport.SendMessage(const aContent: RawUtf8);
var
  msg: RawByteString;
begin
  if not fRunning then
    raise EMCPTransport.Create('Transport not running');

  if not Assigned(fProcess) or not Assigned(fProcess.Input) then
    raise EMCPTransport.Create('Process input not available');

  // Send as NDJSON (plain JSON line with newline terminator)
  msg := aContent + #10;

  DoLog('>>> Sending: %s', [aContent]);

  try
    fProcess.Input.Write(msg[1], Length(msg));
  except
    on E: Exception do
      raise EMCPTransport.CreateFmt('Failed to send message: %s', [E.Message]);
  end;
end;

procedure TMCPStdioTransport.ReaderThreadExecute;
var
  line: RawUtf8;
  c: AnsiChar;
  bytesRead: integer;
begin
  DoLog('Reader thread started');

  try
    while fRunning and not fShuttingDown do
    begin
      // Read line using blocking read (NDJSON format - each message ends with \r\n)
      line := '';
      repeat
        bytesRead := fProcess.Output.Read(c, 1);
        if bytesRead = 0 then
        begin
          // End of stream or error
          if not fProcess.Running then
          begin
            DoLog('Process stopped running (exit code: %d)', [fProcess.ExitCode]);
            Exit;
          end;
          Sleep(10);
          Continue;
        end;

        if c = #10 then
          Break;
        if c <> #13 then
          line := line + c;
      until fShuttingDown;

      // Process complete JSON line
      if (line <> '') and (line[1] = '{') then
      begin
        DoLog('<<< Received: %s', [line]);
        if Assigned(fOnMessage) then
          fOnMessage(line);
      end;
    end;
  except
    on E: Exception do
      DoLog('Reader thread error: %s', [E.Message]);
  end;

  DoLog('Reader thread stopped');
end;

{ TMCPReaderThread }

constructor TMCPReaderThread.Create(aTransport: TMCPStdioTransport);
begin
  fTransport := aTransport;
  FreeOnTerminate := false;
  inherited Create(false);
end;

procedure TMCPReaderThread.Execute;
begin
  fTransport.ReaderThreadExecute;
end;

end.
