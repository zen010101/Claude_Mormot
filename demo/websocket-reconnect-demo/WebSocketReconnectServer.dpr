/// WebSocket Echo Server with Reconnection Demo
/// Based on mORMot2/ex/rest-websockets/restws_simpleechoserver.dpr
program WebSocketReconnectServer;

{$I mormot.defines.inc}

{$APPTYPE CONSOLE}

uses
  {$I mormot.uses.inc}
  SysUtils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.text,
  mormot.core.rtti,
  mormot.net.server,
  mormot.net.ws.core,
  mormot.net.ws.server;

const
  SERVER_PORT = '8999';
  PROTOCOL_NAME = 'echo';  // Must match client

type
  /// Custom echo protocol - inherits from TWebSocketProtocolChat
  TWebSocketProtocolEcho = class(TWebSocketProtocolChat)
  protected
    procedure EchoFrame(Sender: TWebSocketProcess; const Frame: TWebSocketFrame);
  end;

procedure TWebSocketProtocolEcho.EchoFrame(Sender: TWebSocketProcess;
  const Frame: TWebSocketFrame);
begin
  TextColor(ccLightMagenta);
  Write(GetEnumName(TypeInfo(TWebSocketFrameOpCode), ord(Frame.opcode))^, ' - ');
  TextColor(ccWhite);
  case Frame.opcode of
    focContinuation:
      Write('Connected');
    focConnectionClose:
      Write('Disconnected');
    focText, focBinary:
      begin
        Write('Echoing ', Length(Frame.payload), ' bytes');
        SendFrame(Sender, Frame);  // Echo back the same frame
      end;
  end;
  TextColor(ccCyan);
  WriteLn(' from ', Sender.Protocol.RemoteIP, '/', Sender.Protocol.ConnectionID);
end;

procedure Run;
var
  Server: TWebSocketServer;
  Protocol: TWebSocketProtocolEcho;
begin
  Server := TWebSocketServer.Create(SERVER_PORT, nil, nil, 'EchoDemo');
  try
    // Create protocol with name 'echo' and empty URI (matches any path)
    Protocol := TWebSocketProtocolEcho.Create(PROTOCOL_NAME, '');
    Protocol.OnIncomingFrame := Protocol.EchoFrame;
    Server.WebSocketProtocols.Add(Protocol);
    Server.WaitStarted;

    TextColor(ccLightGreen);
    WriteLn('WebSocket Echo Server running on localhost:', SERVER_PORT);
    WriteLn('Protocol name: ', PROTOCOL_NAME);
    WriteLn('');
    TextColor(ccWhite);
    WriteLn('Test with:');
    WriteLn('  - WebSocketReconnectClient.exe (Pascal client)');
    WriteLn('  - Browser: ws://localhost:', SERVER_PORT, '/any/path');
    WriteLn('');
    TextColor(ccLightGray);
    WriteLn('Press [Enter] to quit');
    WriteLn('');
    TextColor(ccCyan);
    ConsoleWaitForEnterKey;
  finally
    Server.Free;
  end;
end;

begin
  try
    Run;
  except
    on E: Exception do
      ConsoleShowFatalException(E);
  end;
end.
