/// WebSocket Client with Reconnection Demo
/// Shows how to use OnReconnect callback for automatic reconnection
program WebSocketReconnectClient;

{$I mormot.defines.inc}

{$APPTYPE CONSOLE}

uses
  {$I mormot.uses.inc}
  SysUtils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.text,
  mormot.core.rtti,
  mormot.net.sock,
  mormot.net.client,
  mormot.net.ws.core,
  mormot.core.unicode,
  mormot.net.ws.client;

const
  SERVER_HOST = 'localhost';
  SERVER_PORT = '8999';
  PROTOCOL_NAME = 'echo';  // Must match server

type
  TWebSocketEchoClient = class
  private
    fClient: THttpClientWebSockets;
    fProtocol: TWebSocketProtocolChat;
    fConnected: boolean;
    fReconnectCount: integer;
    procedure OnIncomingFrame(Sender: TWebSocketProcess; const Frame: TWebSocketFrame);
    procedure OnClosed(Sender: TObject);
    function DoReconnect(aProcess: TWebSocketProcessClient): string;
  public
    constructor Create;
    destructor Destroy; override;
    function Connect: boolean;
    procedure SendMessage(const Msg: RawUtf8);
    procedure Run;
  end;

constructor TWebSocketEchoClient.Create;
begin
  inherited Create;
  fConnected := false;
  fReconnectCount := 0;
end;

destructor TWebSocketEchoClient.Destroy;
begin
  fClient.Free;
  inherited Destroy;
end;

procedure TWebSocketEchoClient.OnIncomingFrame(Sender: TWebSocketProcess;
  const Frame: TWebSocketFrame);
begin
  TextColor(ccLightMagenta);
  Write(GetEnumName(TypeInfo(TWebSocketFrameOpCode), ord(Frame.opcode))^, ' - ');
  TextColor(ccWhite);
  case Frame.opcode of
    focContinuation:
      WriteLn('WebSocket upgraded');
    focConnectionClose:
      WriteLn('Connection closing');
    focText, focBinary:
      begin
        TextColor(ccLightGreen);
        WriteLn('Received: ', Frame.payload);
      end;
  end;
  TextColor(ccLightGray);
end;

procedure TWebSocketEchoClient.OnClosed(Sender: TObject);
begin
  fConnected := false;
  TextColor(ccYellow);
  WriteLn('Connection closed - waiting for reconnection...');
  TextColor(ccLightGray);
end;

function TWebSocketEchoClient.DoReconnect(aProcess: TWebSocketProcessClient): string;
begin
  if aProcess = nil then
  begin
    // Socket ReOpen failed - will retry automatically
    Inc(fReconnectCount);
    fConnected := false;
    TextColor(ccLightRed);
    WriteLn('Reconnection attempt #', fReconnectCount, ' failed');
    TextColor(ccLightGray);
    Result := 'Retry';  // Non-empty = retry with exponential backoff
  end
  else
  begin
    // Socket ReOpen succeeded - now upgrade to WebSocket again!
    // IMPORTANT: ReOpen only reconnects TCP, we must call WebSocketsUpgrade
    // with aReconnect=true to re-establish WebSocket protocol
    Result := fClient.WebSocketsUpgrade(
      aProcess.Protocol.UpgradeUri,  // use same URI as before
      '',                             // encryption key
      false,                          // ajax
      [],                             // binary options
      aProcess.Protocol,              // reuse existing protocol
      '',                             // custom headers
      true);                          // aReconnect = TRUE!

    if Result = '' then
    begin
      fConnected := true;
      fReconnectCount := 0;
      TextColor(ccLightGreen);
      WriteLn('Reconnected! ConnectionID: ', aProcess.ConnectionID);
      TextColor(ccLightGray);
    end
    else
    begin
      fConnected := false;
      TextColor(ccLightRed);
      WriteLn('WebSocket upgrade failed after reconnect: ', Result);
      TextColor(ccLightGray);
    end;
  end;
end;

function TWebSocketEchoClient.Connect: boolean;
var
  err: RawUtf8;
begin
  Result := false;

  WriteLn('Connecting to ', SERVER_HOST, ':', SERVER_PORT, '...');

  // Create HTTP client with WebSocket support
  fClient := THttpClientWebSockets.Open(SERVER_HOST, SERVER_PORT);
  fClient.OnWebSocketsClosed := OnClosed;

  // Create Chat protocol with same name as server
  fProtocol := TWebSocketProtocolChat.Create(PROTOCOL_NAME, '');
  fProtocol.OnIncomingFrame := OnIncomingFrame;

  // Upgrade to WebSocket - pass protocol as 5th parameter
  err := fClient.WebSocketsUpgrade('', '', false, [], fProtocol);
  if err <> '' then
  begin
    TextColor(ccLightRed);
    WriteLn('WebSocket upgrade failed: ', err);
    TextColor(ccLightGray);
    Exit;
  end;

  // Setup automatic reconnection
  fClient.WebSockets.OnReconnect := DoReconnect;

  fConnected := true;
  TextColor(ccLightGreen);
  WriteLn('Connected! ConnectionID: ', fClient.WebSockets.ConnectionID);
  TextColor(ccLightGray);
  Result := true;
end;

procedure TWebSocketEchoClient.SendMessage(const Msg: RawUtf8);
var
  frame: TWebSocketFrame;
begin
  if not fConnected then
  begin
    TextColor(ccLightRed);
    WriteLn('Not connected');
    TextColor(ccLightGray);
    Exit;
  end;

  frame.opcode := focText;
  frame.content := [];
  frame.payload := Msg;

  if fClient.WebSockets.SendFrame(frame) then
  begin
    TextColor(ccCyan);
    WriteLn('Sent: ', Msg);
    TextColor(ccLightGray);
  end
  else
  begin
    TextColor(ccLightRed);
    WriteLn('Failed to send');
    TextColor(ccLightGray);
  end;
end;

procedure TWebSocketEchoClient.Run;
var
  input: string;
begin
  WriteLn('');
  WriteLn('Commands:');
  WriteLn('  Type message + Enter = send message');
  WriteLn('  Empty Enter = check status');
  WriteLn('  Q = quit');
  WriteLn('');
  WriteLn('To test reconnection:');
  WriteLn('  1. Stop server (Enter in server window)');
  WriteLn('  2. Restart server');
  WriteLn('  3. Watch automatic reconnection');
  WriteLn('');

  while True do
  begin
    Write('> ');
    ReadLn(input);

    if (input = 'q') or (input = 'Q') then
      Break;

    if input = '' then
    begin
      if fConnected then
        WriteLn('Status: Connected, ID=', fClient.WebSockets.ConnectionID)
      else
        WriteLn('Status: Disconnected, reconnecting...');
    end
    else
      SendMessage(StringToUtf8(input));
  end;
end;

var
  Client: TWebSocketEchoClient;
begin
  try
    TextColor(ccLightGreen);
    WriteLn('=== WebSocket Echo Client with Reconnection ===');
    TextColor(ccLightGray);
    WriteLn('');

    Client := TWebSocketEchoClient.Create;
    try
      if Client.Connect then
        Client.Run;
    finally
      Client.Free;
    end;
  except
    on E: Exception do
      ConsoleShowFatalException(E);
  end;
end.
