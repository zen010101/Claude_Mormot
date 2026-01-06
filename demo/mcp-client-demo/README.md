# MCP Client Demo

mORMot2 implementation of MCP (Model Context Protocol) client, demonstrating how to communicate with MCP servers using JSON-RPC 2.0 over stdio transport.

## Overview

This demo connects to a [Serena](https://github.com/oraios/serena) MCP server, activates the mORMot2 project, and performs a symbol search.

## Files

| File               | Description                                    |
|--------------------|------------------------------------------------|
| mcp.transport.pas  | Stdio transport layer using TProcess           |
| mcp.client.pas     | JSON-RPC 2.0 protocol and MCP high-level API   |
| MCPClientDemo.dpr  | Demo program                                   |
| MCPClientDemo.lpi  | Lazarus project file                           |
| serena_wrapper.py  | Python wrapper for Windows pipe compatibility  |

## Architecture

```
+----------------+     JSON-RPC 2.0      +------------------+
|  TMCPClient    | <------------------> |  MCP Server      |
|                |      (NDJSON)        |  (e.g. Serena)   |
+----------------+                      +------------------+
       |
       v
+--------------------+
| TMCPStdioTransport |  TProcess stdin/stdout pipes
+--------------------+
       |
       v
+--------------------+
| serena_wrapper.py  |  (Windows only - fixes pipe buffering)
+--------------------+
       |
       v
+--------------------+
| uv run serena ...  |
+--------------------+
```

## Key mORMot2 Features Used

- **IDocDict/IDocList**: JSON object/array handling with fluent interface
- **RawUtf8**: UTF-8 string type for efficient JSON processing
- **TProcess**: Cross-platform subprocess management (FPC)

## Usage

```pascal
var
  Client: TMCPClient;
  tools, args, res: IDocDict;
begin
  Client := TMCPClient.Create;
  try
    Client.Timeout := 60000;

    // Start MCP server
    Client.Start('python', ['serena_wrapper.py'], [], '');
    Sleep(2000);  // Wait for server startup

    // Initialize MCP connection
    Client.Initialize('my-client', '1.0.0');

    // List available tools
    tools := Client.ListTools;

    // Call a tool
    args := DocDict;
    args.U['name_path_pattern'] := 'VariantSaveJson';
    res := Client.CallTool('find_symbol', args);

    // Cleanup
    Client.Stop;
  finally
    Client.Free;
  end;
end;
```

## Building

```bash
# Using lazbuild
lazbuild MCPClientDemo.lpi
```

## Windows Pipe Issue

On Windows, TProcess pipes don't work correctly with subprocess chains like `uv -> python -> serena`. The `Output.Read()` blocks indefinitely and `NumBytesAvailable` always returns 0.

**Workaround**: Use `serena_wrapper.py` which properly handles stdio buffering:

```python
proc = subprocess.Popen(
    ['uv', 'run', 'serena', 'start-mcp-server'],
    stdin=subprocess.PIPE,
    stdout=subprocess.PIPE,
    stderr=subprocess.PIPE,
    text=True,
    bufsize=1,  # Line buffered
)
```

On Linux/macOS, you may be able to call `uv` directly without the wrapper.

## References

- [MCP Specification](https://modelcontextprotocol.io/)
