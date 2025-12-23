# WebSocket Reconnection Demo - mORMot 2

This demo demonstrates proper WebSocket reconnection handling in mORMot 2, addressing common pitfalls and showing the recommended approach.

## Problem

When implementing WebSocket reconnection logic, developers often encounter this error:

```
Already upgraded to WebSockets with URI=[] but requested [/]
```

This happens when:

1. The initial `WebSocketsUpgrade()` call uses an empty URI `''`
2. The reconnection attempt uses a different URI like `'/'`
3. The old `fProcess` instance still exists in the client

**Forum Discussion:** https://synopse.info/forum/viewtopic.php?id=7477 (deleted by author)

## Root Cause Analysis

In `mormot.net.ws.client.pas`, the `WebSocketsUpgrade` method checks:

```pascal
if not aReconnect then
  if fProcess <> nil then
  begin
    result := 'Already upgraded to WebSockets';
    if PropNameEquals(fProcess.Protocol.Uri, aWebSocketsURI) then
      result := result + ' on this URI'
    else
      result := FormatUtf8('% with URI=[%] but requested [%]',
        [result, fProcess.Protocol.Uri, aWebSocketsURI]);
    exit;
  end;
```

The key insight: `aReconnect = true` bypasses the URI/process check, allowing reconnection to proceed.
This is the foundation for the automatic reconnection mechanism that mORMot provides.

## Solution: Use Built-in OnReconnect Callback

mORMot 2 has a built-in automatic reconnection mechanism. **This is the only solution that supports automatic background reconnection**.

**IMPORTANT:** The `OnReconnect` callback receives notification that TCP socket is reopened, but you **MUST call `WebSocketsUpgrade` with `aReconnect=true`** to re-establish the WebSocket protocol layer.

```pascal
function DoReconnect(aProcess: TWebSocketProcessClient): string;
begin
  if aProcess = nil then
  begin
    // Socket ReOpen failed - will retry automatically
    Inc(fReconnectCount);
    fConnected := false;
    WriteLn('Reconnection attempt #', fReconnectCount, ' failed');
    Result := 'Retry';  // Non-empty = retry with exponential backoff
  end
  else
  begin
    // Socket ReOpen succeeded - now upgrade to WebSocket again!
    // CRITICAL: ReOpen only reconnects TCP, we must call WebSocketsUpgrade
    Result := fClient.WebSocketsUpgrade(
      aProcess.Protocol.UpgradeUri,    // use same URI as before
      '',                               // encryption key
      false,                            // ajax
      [],                               // binary options
      aProcess.Protocol,                // reuse existing protocol
      '',                               // custom headers
      true);                            // aReconnect = TRUE!

    if Result = '' then
    begin
      fConnected := true;
      fReconnectCount := 0;
      WriteLn('Reconnected! ConnectionID: ', aProcess.ConnectionID);
      // Re-register subscriptions, send auth, etc. here
      Result := '';  // Empty = success, continue
    end
    else
    begin
      fConnected := false;
      WriteLn('WebSocket upgrade failed after reconnect: ', Result);
      Result := 'Retry';  // Non-empty = retry later
    end;
  end;
end;

// Register the callback during initial connection setup
Client.WebSockets.OnReconnect := DoReconnect;
```

The background thread (`TWebSocketProcessClientThread`) automatically:
1. Detects connection loss (via heartbeat timeout)
2. Calls `ReOpen` to reconnect the TCP socket
3. Invokes `OnReconnect` callback for application-level WebSocket upgrade
4. Retries with exponential backoff (up to 10-15 seconds)

## Architecture

```
┌─────────────────────┐         ┌─────────────────────┐
│  WebSocket Client   │   WS    │  WebSocket Server   │
│                     │◄───────►│                     │
│  - OnReconnect      │         │  - Echo messages    │
│  - Auto retry       │         │  - Heartbeat        │
│  - Heartbeat        │         │  - Connection mgmt  │
└─────────────────────┘         └─────────────────────┘
         │                               │
         │  Connection lost              │
         │  ───────────X                 │
         │                               │
         │  Auto reconnect               │
         │  ─────────────────────────────►
         │                               │
```

## Files

| File                          | Description                              |
|-------------------------------|------------------------------------------|
| `WebSocketReconnectServer.dpr`| WebSocket echo server                    |
| `WebSocketReconnectServer.lpi`| Lazarus project for server               |
| `WebSocketReconnectClient.dpr`| Client demonstrating reconnection        |
| `WebSocketReconnectClient.lpi`| Lazarus project for client               |

## Building

### With Free Pascal / Lazarus

```bash
# Build server
lazbuild WebSocketReconnectServer.lpi

# Build client
lazbuild WebSocketReconnectClient.lpi
```

### With Delphi

Open the `.dpr` files in Delphi IDE and compile.

## Usage

1. **Start the server:**
   ```
   WebSocketReconnectServer.exe
   ```
   Server listens on port 8999 with protocol name "echo".

2. **Run the client:**
   ```
   WebSocketReconnectClient.exe
   ```

3. **Test echo:**
   - Type a message and press Enter to send
   - Server echoes the message back
   - Empty Enter shows connection status

4. **Test reconnection:**
   - Stop the server (Enter in server window) to simulate disconnection
   - Watch "[CLOSED]" message appear in client
   - Restart the server
   - Watch the client automatically reconnect via `OnReconnect` callback
   - Press 'Q' to quit

## Implementation Notes

Based on `mORMot2/ex/rest-websockets/restws_simpleechoserver.dpr`:

```pascal
// Server: Create custom protocol class
TWebSocketProtocolEcho = class(TWebSocketProtocolChat)
  procedure EchoFrame(Sender: TWebSocketProcess; const Frame: TWebSocketFrame);
end;

// Server: Register protocol with name and empty URI
Protocol := TWebSocketProtocolEcho.Create('echo', '');  // name, uri
Protocol.OnIncomingFrame := Protocol.EchoFrame;
Server.WebSocketProtocols.Add(Protocol);  // Use Add, not AddOnce

// Client: Create matching protocol and upgrade
fProtocol := TWebSocketProtocolChat.Create('echo', '');  // Same name!
fProtocol.OnIncomingFrame := OnIncomingFrame;
fClient.WebSocketsUpgrade('', '', false, [], fProtocol);  // Pass protocol as 5th param
```

Key points:
- Protocol **name must match** between client and server ('echo')
- Protocol **URI can be empty** `''` to match any request path
- Use `WebSocketProtocols.Add()` not `AddOnce()`
- Client passes custom protocol as 5th parameter to `WebSocketsUpgrade`

## Key Takeaways

1. **Use `OnReconnect` callback for automatic reconnection** - This is the only approach that supports automatic background reconnection with exponential backoff
2. **In `OnReconnect`: MUST call `WebSocketsUpgrade` with `aReconnect=true`** - TCP reconnect (via `ReOpen`) is separate from WebSocket protocol upgrade
3. **Use consistent URIs** - Don't mix `''` and `'/'` between initial connection and reconnection
4. **Handle both success and failure** in `OnReconnect` callback - Return `'Retry'` on failure, empty string on success
5. **Don't Free client in callbacks** - This causes deadlock/freezing

## Common Mistakes

### Mistake 1: Freeing Client During Reconnection

```pascal
// WRONG - causes freeze
Client.WebSockets.OnReconnect :=
  function(aProcess: TWebSocketProcessClient): string
  begin
    if aProcess = nil then
      FreeAndNil(Client);  // DON'T DO THIS!
  end;
```

### Mistake 2: Inconsistent URI

```pascal
// WRONG - URI mismatch
Client.WebSocketsUpgrade('', ...);     // Initial: empty
Client.WebSocketsUpgrade('/', ...);    // Reconnect: slash
```

### Mistake 3: Not Calling WebSocketsUpgrade in OnReconnect Callback

```pascal
// WRONG - OnReconnect doesn't re-establish WebSocket protocol
Client.WebSockets.OnReconnect :=
  function(aProcess: TWebSocketProcessClient): string
  begin
    if aProcess <> nil then
      WriteLn('Reconnected!');
      // TCP socket is reconnected, but WebSocket is NOT!
      // Messages will fail to send/receive
    Result := '';
  end;

// CORRECT - Must call WebSocketsUpgrade in OnReconnect
Client.WebSockets.OnReconnect :=
  function(aProcess: TWebSocketProcessClient): string
  begin
    if aProcess <> nil then
    begin
      // Re-establish WebSocket protocol after TCP reconnect
      Result := Client.WebSocketsUpgrade(
        aProcess.Protocol.UpgradeUri, '', false, [],
        aProcess.Protocol, '', true);  // aReconnect = TRUE
      if Result = '' then
        WriteLn('Reconnected and WebSocket upgraded!');
    end;
  end;
```

## License

This demo is part of the mORMot 2 framework examples.
See https://github.com/synopse/mORMot2 for licensing information.
