/// Async Task Demo - Server
// - demonstrates handling long-running operations without timeout issues
program AsyncTaskServer;

{$I mormot.defines.inc}

{$APPTYPE CONSOLE}

uses
  {$I mormot.uses.inc}
  SysUtils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.log,
  mormot.core.json,
  mormot.orm.core,
  mormot.soa.core,
  mormot.soa.server,
  mormot.rest.core,
  mormot.rest.server,
  mormot.rest.memserver,
  mormot.rest.http.server,
  AsyncTaskInterfaces,
  AsyncTaskManager,
  AsyncTaskServiceImpl;

var
  Model: TOrmModel;
  RestServer: TRestServerFullMemory;
  HttpServer: TRestHttpServer;

begin
  // Setup logging
  with TSynLog.Family do
  begin
    Level := LOG_VERBOSE;
    EchoToConsole := LOG_VERBOSE;
    PerThreadLog := ptIdentifiedInOneFile;
  end;

  try
    WriteLn('===========================================');
    WriteLn('  Async Task Demo Server');
    WriteLn('  Demonstrates long-running task handling');
    WriteLn('===========================================');
    WriteLn;

    // Create ORM model (empty - we only use services)
    Model := TOrmModel.Create([], 'root');

    // Create REST server
    RestServer := TRestServerFullMemory.Create(Model, False);
    try
      // Register the async task service
      // Using sicClientDriven with normal timeout - polling keeps session alive
      RestServer.ServiceDefine(TAsyncTaskService, [IAsyncTaskService], sicClientDriven)
        .SetTimeoutSec(60); // 60 seconds timeout - short for demo

      WriteLn('Service registered with 60 second timeout');
      WriteLn('(Polling via GetTaskResult keeps session alive)');
      WriteLn;

      // Create HTTP server
      HttpServer := TRestHttpServer.Create(
        '8888',
        [RestServer],
        '+',
        HTTP_DEFAULT_MODE);
      try
        HttpServer.AccessControlAllowOrigin := '*';

        WriteLn('HTTP Server started on port 8888');
        WriteLn;
        WriteLn('Press Enter to stop...');
        WriteLn;
        ReadLn;

      finally
        HttpServer.Free;
      end;
    finally
      RestServer.Free;
      Model.Free;
    end;

    WriteLn('Server stopped.');
  except
    on E: Exception do
    begin
      WriteLn('ERROR: ', E.Message);
      ReadLn;
    end;
  end;
end.
