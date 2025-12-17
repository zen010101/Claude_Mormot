/// Async Task Demo - Client
// - demonstrates polling pattern for long-running operations
program AsyncTaskClient;

{$I mormot.defines.inc}

{$APPTYPE CONSOLE}

uses
  {$I mormot.uses.inc}
  SysUtils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.log,
  mormot.core.json,
  mormot.core.text,
  mormot.orm.core,
  mormot.soa.core,
  mormot.soa.client,
  mormot.rest.core,
  mormot.rest.client,
  mormot.rest.http.client,
  AsyncTaskInterfaces;

procedure RunDemo;
var
  Model: TOrmModel;
  Client: TRestHttpClient;
  Service: IAsyncTaskService;
  TaskID: Int64;
  Result: TTaskResult;
  Duration: Integer;
  LastProgress: Integer;
begin
  WriteLn('===========================================');
  WriteLn('  Async Task Demo Client');
  WriteLn('  Demonstrates polling pattern');
  WriteLn('===========================================');
  WriteLn;

  // Create model and client
  Model := TOrmModel.Create([], 'root');
  try
    Client := TRestHttpClient.Create('127.0.0.1', '8888', Model);
    try
      // Register service interface on client side
      Client.ServiceDefine([IAsyncTaskService], sicClientDriven);
      // Resolve service interface
      if not Client.Services.Resolve(IAsyncTaskService, Service) then
      begin
        WriteLn('ERROR: Cannot resolve IAsyncTaskService');
        WriteLn('Make sure the server is running!');
        Exit;
      end;

      WriteLn('Connected to server');
      WriteLn;

      // Ask for task duration
      Write('Enter task duration in seconds (1-60): ');
      ReadLn(Duration);
      if Duration < 1 then
        Duration := 5;
      if Duration > 60 then
        Duration := 60;

      WriteLn;
      WriteLn('Starting task with ', Duration, ' second duration...');
      WriteLn('(Server timeout is 60s, but polling keeps session alive)');
      WriteLn;

      // Start the long-running task
      TaskID := Service.StartTask(Duration, 'DemoTask');
      WriteLn('Task started with ID: ', TaskID);
      WriteLn;

      // Poll for results
      // This is the key: each GetTaskResult call updates LastAccessTix10
      // keeping the session alive even during long operations
      LastProgress := -1;
      repeat
        SleepHiRes(500); // Poll every 500ms
        Result := Service.GetTaskResult(TaskID);

        // Show progress only when changed
        if Result.Progress <> LastProgress then
        begin
          LastProgress := Result.Progress;
          case Result.Status of
            tsQueued:
              WriteLn('Status: Queued...');
            tsRunning:
              WriteLn('Status: Running... Progress: ', Result.Progress, '%');
            tsCompleted:
              begin
                WriteLn('Status: COMPLETED!');
                WriteLn('Result: ', Result.ResultJson);
              end;
            tsFailed:
              WriteLn('Status: FAILED! Error: ', Result.ErrorMessage);
            tsCancelled:
              WriteLn('Status: CANCELLED! ', Result.ErrorMessage);
          end;
        end;
      until Result.Status in [tsCompleted, tsFailed, tsCancelled];

      WriteLn;
      WriteLn('Task finished!');
      WriteLn;

      // Show active tasks
      WriteLn('Active tasks on server: ', Length(Service.ListActiveTasks));

    finally
      Service := nil;
      Client.Free;
    end;
  finally
    Model.Free;
  end;
end;

begin
  // Setup logging
  with TSynLog.Family do
  begin
    Level := LOG_STACKTRACE;
    EchoToConsole := LOG_STACKTRACE;
  end;

  try
    RunDemo;
  except
    on E: Exception do
      WriteLn('ERROR: ', E.Message);
  end;

  WriteLn;
  WriteLn('Press Enter to exit...');
  ReadLn;
end.
