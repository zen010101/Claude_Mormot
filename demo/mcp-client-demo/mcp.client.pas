/// MCP Client Implementation
// - JSON-RPC 2.0 protocol layer
// - MCP high-level API (initialize, tools, resources, prompts)
// - Synchronous request/response with timeout
// - Uses IDocDict/IDocList for JSON handling
unit mcp.client;

{$I mormot.defines.inc}

{$ifdef MSWINDOWS}
  {$APPTYPE CONSOLE}
{$endif}

interface

uses
  {$I mormot.uses.inc}
  Classes,
  SysUtils,
  SyncObjs,
  mormot.core.base,
  mormot.core.os,
  mormot.core.text,
  mormot.core.unicode,
  mormot.core.variants,
  mormot.core.json,
  mormot.core.rtti,
  mcp.transport;

const
  /// Default timeout for requests in milliseconds
  MCP_DEFAULT_TIMEOUT = 30000;

  /// JSON-RPC version
  JSONRPC_VERSION = '2.0';

  /// MCP protocol version
  MCP_PROTOCOL_VERSION = '2024-11-05';

type
  /// Exception raised for MCP client errors
  EMCPClient = class(Exception);

  /// Exception raised for JSON-RPC errors
  EJSONRPCError = class(EMCPClient)
  public
    Code: integer;
    constructor Create(aCode: integer; const aMessage: string);
  end;

  /// Pending request waiting for response
  TMCPPendingRequest = class
  private
    fID: integer;
    fMethod: RawUtf8;
    fEvent: TEvent;
    fResponse: IDocDict;
    fError: RawUtf8;
    fCompleted: boolean;
  public
    constructor Create(aID: integer; const aMethod: RawUtf8);
    destructor Destroy; override;
    /// Wait for the response with timeout
    function WaitFor(aTimeoutMs: integer): boolean;
    property ID: integer read fID;
    property Method: RawUtf8 read fMethod;
    property Response: IDocDict read fResponse write fResponse;
    property Error: RawUtf8 read fError write fError;
    property Completed: boolean read fCompleted write fCompleted;
  end;

  /// MCP Client for communicating with MCP servers
  // - Implements JSON-RPC 2.0 over stdio transport
  // - Provides high-level MCP protocol methods
  // - Uses IDocDict/IDocList for JSON data
  TMCPClient = class
  private
    fTransport: TMCPStdioTransport;
    fOwnsTransport: boolean;
    fRequestID: integer;
    fPendingRequests: TStringList; // ID -> TMCPPendingRequest
    fLock: TCriticalSection;
    fTimeout: integer;
    fServerInfo: IDocDict;
    fServerCapabilities: IDocDict;
    fInitialized: boolean;
    fOnLog: TOnMCPLog;
    procedure HandleMessage(const aMessage: RawUtf8);
    procedure HandleResponse(const aDoc: IDocDict);
    procedure HandleNotification(const aDoc: IDocDict);
    function GetNextID: integer;
    function FindPendingRequest(aID: integer): TMCPPendingRequest;
    procedure RemovePendingRequest(aID: integer);
    procedure DoLog(const aMsg: string); overload;
    procedure DoLog(const aFmt: string; const aArgs: array of const); overload;
  public
    /// Create with external transport
    constructor Create(aTransport: TMCPStdioTransport; aOwnsTransport: boolean = false); overload;
    /// Create with built-in transport
    constructor Create; overload;
    destructor Destroy; override;

    /// Configure and start the MCP server
    procedure Start(const aCommand: TFileName; const aArgs: array of string;
      const aEnv: array of string; const aWorkDir: TFileName = '');

    /// Stop the MCP server
    procedure Stop;

    /// Send a JSON-RPC request and wait for response
    // - Returns the 'result' field from the response as IDocDict
    // - Raises EJSONRPCError on error response
    function SendRequest(const aMethod: RawUtf8; const aParams: IDocDict): IDocDict; overload;
    function SendRequest(const aMethod: RawUtf8): IDocDict; overload;

    /// Send a JSON-RPC notification (no response expected)
    procedure SendNotification(const aMethod: RawUtf8; const aParams: IDocDict); overload;
    procedure SendNotification(const aMethod: RawUtf8); overload;

    // === MCP High-Level API ===

    /// Initialize the MCP connection
    // - Must be called before any other MCP methods
    procedure Initialize(const aClientName: RawUtf8 = 'mormot-mcp-client';
      const aClientVersion: RawUtf8 = '1.0.0');

    /// Shutdown the MCP connection gracefully
    procedure Shutdown;

    /// List available tools from the server
    function ListTools: IDocDict;

    /// Call a tool with the given arguments
    function CallTool(const aToolName: RawUtf8; const aArguments: IDocDict): IDocDict;

    /// List available resources from the server
    function ListResources: IDocDict;

    /// Read a resource by URI
    function ReadResource(const aUri: RawUtf8): IDocDict;

    /// List available prompts from the server
    function ListPrompts: IDocDict;

    /// Get a prompt by name with optional arguments
    function GetPrompt(const aName: RawUtf8; const aArguments: IDocDict): IDocDict;

    /// Transport instance
    property Transport: TMCPStdioTransport read fTransport;
    /// Request timeout in milliseconds
    property Timeout: integer read fTimeout write fTimeout;
    /// Server info from initialize response
    property ServerInfo: IDocDict read fServerInfo;
    /// Server capabilities from initialize response
    property ServerCapabilities: IDocDict read fServerCapabilities;
    /// Whether the client has been initialized
    property Initialized: boolean read fInitialized;
    /// Log callback
    property OnLog: TOnMCPLog read fOnLog write fOnLog;
  end;

implementation

{ EJSONRPCError }

constructor EJSONRPCError.Create(aCode: integer; const aMessage: string);
begin
  inherited Create(aMessage);
  Code := aCode;
end;

{ TMCPPendingRequest }

constructor TMCPPendingRequest.Create(aID: integer; const aMethod: RawUtf8);
begin
  inherited Create;
  fID := aID;
  fMethod := aMethod;
  fEvent := TEvent.Create(nil, true, false, '');
  fCompleted := false;
end;

destructor TMCPPendingRequest.Destroy;
begin
  fEvent.Free;
  inherited Destroy;
end;

function TMCPPendingRequest.WaitFor(aTimeoutMs: integer): boolean;
begin
  Result := fEvent.WaitFor(aTimeoutMs) = wrSignaled;
end;

{ TMCPClient }

constructor TMCPClient.Create(aTransport: TMCPStdioTransport; aOwnsTransport: boolean);
begin
  inherited Create;
  fTransport := aTransport;
  fOwnsTransport := aOwnsTransport;
  fTransport.OnMessage := HandleMessage;
  fPendingRequests := TStringList.Create;
  fPendingRequests.OwnsObjects := true;
  fLock := TCriticalSection.Create;
  fTimeout := MCP_DEFAULT_TIMEOUT;
  fRequestID := 0;
  fInitialized := false;
end;

constructor TMCPClient.Create;
begin
  Create(TMCPStdioTransport.Create, true);
end;

destructor TMCPClient.Destroy;
begin
  Stop;
  fLock.Free;
  fPendingRequests.Free;
  if fOwnsTransport then
    fTransport.Free;
  inherited Destroy;
end;

procedure TMCPClient.DoLog(const aMsg: string);
begin
  if Assigned(fOnLog) then
    fOnLog(aMsg);
end;

procedure TMCPClient.DoLog(const aFmt: string; const aArgs: array of const);
begin
  DoLog(Format(aFmt, aArgs));
end;

function TMCPClient.GetNextID: integer;
begin
  fLock.Enter;
  try
    Inc(fRequestID);
    Result := fRequestID;
  finally
    fLock.Leave;
  end;
end;

function TMCPClient.FindPendingRequest(aID: integer): TMCPPendingRequest;
var
  idx: integer;
begin
  Result := nil;
  fLock.Enter;
  try
    idx := fPendingRequests.IndexOf(IntToStr(aID));
    if idx >= 0 then
      Result := TMCPPendingRequest(fPendingRequests.Objects[idx]);
  finally
    fLock.Leave;
  end;
end;

procedure TMCPClient.RemovePendingRequest(aID: integer);
var
  idx: integer;
begin
  fLock.Enter;
  try
    idx := fPendingRequests.IndexOf(IntToStr(aID));
    if idx >= 0 then
      fPendingRequests.Delete(idx);
  finally
    fLock.Leave;
  end;
end;

procedure TMCPClient.HandleMessage(const aMessage: RawUtf8);
var
  doc: IDocDict;
  v: variant;
begin
  // Parse JSON string to variant first, then wrap as IDocDict
  if not _Json(aMessage, v, JSON_FAST) then
  begin
    DoLog('Failed to parse JSON: %s', [Copy(aMessage, 1, 200)]);
    Exit;
  end;
  doc := DocDictFrom(v);
  if doc = nil then
  begin
    DoLog('Failed to create IDocDict from: %s', [Copy(aMessage, 1, 200)]);
    Exit;
  end;

  // Check if it's a response (has 'id' and either 'result' or 'error')
  if doc.Exists('id') and (doc.Exists('result') or doc.Exists('error')) then
    HandleResponse(doc)
  // Check if it's a notification (has 'method' but no 'id')
  else if doc.Exists('method') and not doc.Exists('id') then
    HandleNotification(doc)
  else
    DoLog('Unknown message type: %s', [aMessage]);
end;

procedure TMCPClient.HandleResponse(const aDoc: IDocDict);
var
  id: integer;
  pending: TMCPPendingRequest;
  errorObj: IDocDict;
begin
  id := aDoc.I['id'];
  pending := FindPendingRequest(id);

  if pending = nil then
  begin
    DoLog('Received response for unknown request ID: %d', [id]);
    Exit;
  end;

  // Check for error
  if aDoc.Exists('error') then
  begin
    errorObj := aDoc.D['error'];
    if errorObj <> nil then
      pending.Error := FormatUtf8('Error %: %',
        [errorObj.I['code'], errorObj.U['message']])
    else
      pending.Error := 'Unknown error';
  end
  else
  begin
    // Get result - use Copy to create independent deep copy
    // (DocDictFrom creates weak reference that becomes invalid after HandleMessage exits)
    if aDoc.D['result'] <> nil then
      pending.Response := aDoc.D['result'].Copy
    else
      pending.Response := nil;
  end;

  pending.Completed := true;
  pending.fEvent.SetEvent;
end;

procedure TMCPClient.HandleNotification(const aDoc: IDocDict);
var
  method: RawUtf8;
begin
  method := aDoc.U['method'];
  DoLog('Received notification: %s', [method]);
  // TODO: Handle specific notifications if needed
end;

procedure TMCPClient.Start(const aCommand: TFileName; const aArgs: array of string;
  const aEnv: array of string; const aWorkDir: TFileName);
var
  i: integer;
begin
  fTransport.Command := aCommand;
  fTransport.Arguments.Clear;
  for i := 0 to High(aArgs) do
    fTransport.Arguments.Add(aArgs[i]);
  fTransport.Environment.Clear;
  for i := 0 to High(aEnv) do
    fTransport.Environment.Add(aEnv[i]);
  fTransport.WorkingDir := aWorkDir;
  fTransport.Start;
end;

procedure TMCPClient.Stop;
begin
  if fInitialized then
  begin
    try
      Shutdown;
    except
      // Ignore errors during shutdown
    end;
  end;
  fTransport.Stop;
  fInitialized := false;
end;

function TMCPClient.SendRequest(const aMethod: RawUtf8; const aParams: IDocDict): IDocDict;
var
  id: integer;
  request: IDocDict;
  pending: TMCPPendingRequest;
  json: RawUtf8;
begin
  if not fTransport.Running then
    raise EMCPClient.Create('Transport not running');

  id := GetNextID;

  // Build JSON-RPC request
  request := DocDict;
  request.U['jsonrpc'] := JSONRPC_VERSION;
  request.U['method'] := aMethod;
  request.I['id'] := id;
  if (aParams <> nil) and (aParams.Len > 0) then
    request['params'] := aParams.AsVariant;

  // Create pending request
  pending := TMCPPendingRequest.Create(id, aMethod);
  fLock.Enter;
  try
    fPendingRequests.AddObject(IntToStr(id), pending);
  finally
    fLock.Leave;
  end;

  try
    // Send request
    json := request.Json;
    fTransport.SendMessage(json);

    // Wait for response
    if not pending.WaitFor(fTimeout) then
    begin
      RemovePendingRequest(id);
      raise EMCPClient.CreateFmt('Request timeout: %s', [aMethod]);
    end;

    // Check for error
    if pending.Error <> '' then
      raise EMCPClient.Create(Utf8ToString(pending.Error));

    // Return result
    Result := pending.Response;
  finally
    RemovePendingRequest(id);
  end;
end;

function TMCPClient.SendRequest(const aMethod: RawUtf8): IDocDict;
begin
  Result := SendRequest(aMethod, nil);
end;

procedure TMCPClient.SendNotification(const aMethod: RawUtf8; const aParams: IDocDict);
var
  notification: IDocDict;
  json: RawUtf8;
begin
  if not fTransport.Running then
    raise EMCPClient.Create('Transport not running');

  // Build JSON-RPC notification (no id)
  notification := DocDict;
  notification.U['jsonrpc'] := JSONRPC_VERSION;
  notification.U['method'] := aMethod;
  if (aParams <> nil) and (aParams.Len > 0) then
    notification['params'] := aParams.AsVariant;

  json := notification.Json;
  fTransport.SendMessage(json);
end;

procedure TMCPClient.SendNotification(const aMethod: RawUtf8);
begin
  SendNotification(aMethod, nil);
end;

// === MCP High-Level API ===

procedure TMCPClient.Initialize(const aClientName: RawUtf8; const aClientVersion: RawUtf8);
var
  params: IDocDict;
  clientInfo: IDocDict;
  capabilities: IDocDict;
  response: IDocDict;
begin
  // Build initialize params
  params := DocDict;
  params.U['protocolVersion'] := MCP_PROTOCOL_VERSION;

  clientInfo := DocDict;
  clientInfo.U['name'] := aClientName;
  clientInfo.U['version'] := aClientVersion;
  params['clientInfo'] := clientInfo.AsVariant;

  capabilities := DocDict;
  params['capabilities'] := capabilities.AsVariant;

  // Send initialize request
  response := SendRequest('initialize', params);

  // Store server info and capabilities
  if (response <> nil) then
  begin
    if response.Exists('serverInfo') then
      fServerInfo := response.D['serverInfo'];
    if response.Exists('capabilities') then
      fServerCapabilities := response.D['capabilities'];
  end;

  // Send initialized notification
  SendNotification('notifications/initialized');

  fInitialized := true;
  if fServerInfo <> nil then
    DoLog('MCP initialized. Server: %s', [fServerInfo.Json])
  else
    DoLog('MCP initialized.');
end;

procedure TMCPClient.Shutdown;
begin
  if not fInitialized then
    Exit;

  // MCP doesn't have a formal shutdown, just stop
  fInitialized := false;
end;

function TMCPClient.ListTools: IDocDict;
begin
  Result := SendRequest('tools/list');
end;

function TMCPClient.CallTool(const aToolName: RawUtf8; const aArguments: IDocDict): IDocDict;
var
  params: IDocDict;
begin
  params := DocDict;
  params.U['name'] := aToolName;
  if aArguments <> nil then
    params['arguments'] := aArguments.AsVariant;
  Result := SendRequest('tools/call', params);
end;

function TMCPClient.ListResources: IDocDict;
begin
  Result := SendRequest('resources/list');
end;

function TMCPClient.ReadResource(const aUri: RawUtf8): IDocDict;
var
  params: IDocDict;
begin
  params := DocDict;
  params.U['uri'] := aUri;
  Result := SendRequest('resources/read', params);
end;

function TMCPClient.ListPrompts: IDocDict;
begin
  Result := SendRequest('prompts/list');
end;

function TMCPClient.GetPrompt(const aName: RawUtf8; const aArguments: IDocDict): IDocDict;
var
  params: IDocDict;
begin
  params := DocDict;
  params.U['name'] := aName;
  if aArguments <> nil then
    params['arguments'] := aArguments.AsVariant;
  Result := SendRequest('prompts/get', params);
end;

end.
