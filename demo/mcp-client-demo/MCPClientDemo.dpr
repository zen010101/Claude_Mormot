/// MCP Client Demo - Connect to Serena MCP Server
// - Demonstrates MCP protocol communication using mORMot2
// - Connects to serena, activates mormot2 project, finds symbol
// - Source: https://synopse.info/forum/viewtopic.php?id=7482
program MCPClientDemo;

{$I mormot.defines.inc}

{$ifdef MSWINDOWS}
  {$APPTYPE CONSOLE}
{$endif}

uses
  {$I mormot.uses.inc}
  Classes,
  SysUtils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.text,
  mormot.core.unicode,
  mormot.core.variants,
  mormot.core.json,
  mcp.transport,
  mcp.client;

var
  Client: TMCPClient;

procedure Log(const aMsg: RawUtf8);
begin
  WriteLn(FormatDateTime('hh:nn:ss.zzz', Now), ' ', aMsg);
end;

procedure OnClientLog(const aMsg: string);
begin
  Log(StringToUtf8(aMsg));
end;

procedure PrintTools(const aTools: IDocDict);
var
  tools: IDocList;
  i: integer;
  tool: IDocDict;
  desc: RawUtf8;
begin
  if aTools = nil then
  begin
    WriteLn('No result');
    Exit;
  end;

  tools := aTools.L['tools'];
  if tools = nil then
  begin
    WriteLn('No tools found');
    Exit;
  end;

  WriteLn('Available tools (', tools.Len, '):');
  WriteLn('----------------------------------------');
  for i := 0 to tools.Len - 1 do
  begin
    tool := tools.D[i];
    if tool <> nil then
    begin
      WriteLn(FormatUtf8('  [%] %', [i + 1, tool.U['name']]));
      desc := tool.U['description'];
      if desc <> '' then
      begin
        if Length(desc) > 60 then
          desc := Copy(desc, 1, 60) + '...';
        WriteLn('      ', desc);
      end;
    end;
  end;
  WriteLn('----------------------------------------');
end;

procedure PrintResult(const aResult: IDocDict);
var
  content: IDocList;
  i: integer;
  item: IDocDict;
begin
  if aResult = nil then
  begin
    WriteLn('Result: nil');
    Exit;
  end;

  content := aResult.L['content'];
  if content = nil then
  begin
    WriteLn('Result: ', aResult.Json);
    Exit;
  end;

  WriteLn('Result:');
  WriteLn('========================================');
  for i := 0 to content.Len - 1 do
  begin
    item := content.D[i];
    if (item <> nil) and (item.U['type'] = 'text') then
      WriteLn(item.U['text']);
  end;
  WriteLn('========================================');
end;

procedure RunDemo;
var
  tools: IDocDict;
  args: IDocDict;
  res: IDocDict;
begin
  Client := TMCPClient.Create;
  try
    Client.OnLog := OnClientLog;
    Client.Transport.OnLog := OnClientLog;
    Client.Timeout := 60000; // 60 seconds for slow operations

    WriteLn('=== MCP Client Demo ===');
    WriteLn;

    // Start serena MCP server via Python wrapper
    // Note: Direct uv subprocess doesn't work with TProcess pipes on Windows,
    // so we use a Python wrapper that properly handles stdio buffering
    WriteLn('Starting Serena MCP server (via Python wrapper)...');
    Client.Start(
      'python',  // Command
      ['D:/Gits/Claude_Mormot/demo/mcp-client-demo/serena_wrapper.py'],  // Arguments
      ['PYTHONUNBUFFERED=1',
       'PP=D:/laz32/fpc/bin/i386-win32/ppc386.exe',
       'FPCDIR=D:/laz32/fpc',
       'LAZARUSDIR=D:/laz32/lazarus'],  // Environment
      ''  // Working directory
    );

    // Wait a bit for the server to start
    Sleep(2000);

    // Initialize MCP connection
    WriteLn;
    WriteLn('Initializing MCP connection...');
    Client.Initialize('mormot-mcp-client', '1.0.0');
    if Client.ServerInfo <> nil then
      WriteLn('Server info: ', Client.ServerInfo.Json);
    WriteLn;

    // List available tools
    WriteLn('Listing available tools...');
    tools := Client.ListTools;
    PrintTools(tools);
    WriteLn;

    // Activate mormot2 project
    WriteLn('Activating mormot2 project...');
    args := DocDict;
    args.U['project'] := 'mORMot2';
    res := Client.CallTool('activate_project', args);
    PrintResult(res);
    WriteLn;

    // Find symbol: VariantSaveJson
    WriteLn('Finding symbol: VariantSaveJson...');
    args := DocDict;
    args.U['name_path_pattern'] := 'VariantSaveJson';
    args.B['include_body'] := false;
    res := Client.CallTool('find_symbol', args);
    PrintResult(res);
    WriteLn;

    // Cleanup
    WriteLn('Shutting down...');
    Client.Stop;

    WriteLn;
    WriteLn('Demo completed successfully!');

  finally
    Client.Free;
  end;
end;

begin
  try
    RunDemo;
  except
    on E: Exception do
    begin
      WriteLn('ERROR: ', E.ClassName, ': ', E.Message);
      ExitCode := 1;
    end;
  end;

  WriteLn;
  WriteLn('Press Enter to exit...');
  ReadLn;
end.
