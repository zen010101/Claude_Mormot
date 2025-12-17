/// Service Implementation for Async Task Demo
unit AsyncTaskServiceImpl;

interface

{$I mormot.defines.inc}

uses
  SysUtils,
  mormot.core.base,
  mormot.core.data,
  mormot.core.text,
  mormot.core.os,
  mormot.core.json,
  mormot.core.interfaces,
  mormot.soa.core,
  mormot.soa.server,
  AsyncTaskInterfaces,
  AsyncTaskManager;

type
  /// Service implementation
  TAsyncTaskService = class(TInjectableObject, IAsyncTaskService)
  public
    function StartTask(DurationSeconds: Integer;
      const TaskName: RawUtf8): Int64;
    function GetTaskResult(TaskID: Int64): TTaskResult;
    function CancelTask(TaskID: Int64): Boolean;
    function ListActiveTasks: TInt64DynArray;
  end;

implementation

{ TAsyncTaskService }

function TAsyncTaskService.StartTask(DurationSeconds: Integer;
  const TaskName: RawUtf8): Int64;
var
  name: RawUtf8;
begin
  if DurationSeconds < 1 then
    DurationSeconds := 1;
  if DurationSeconds > 300 then
    DurationSeconds := 300; // Max 5 minutes for demo
  name := TaskName;
  if name = '' then
    name := FormatUtf8('Task_%', [GetTickCount64]);
  Result := TaskManager.CreateTask(DurationSeconds, name);
end;

function TAsyncTaskService.GetTaskResult(TaskID: Int64): TTaskResult;
begin
  // This method keeps the session alive because each call
  // updates LastAccessTix10 in TServiceFactoryServer.RetrieveInstance
  Result := TaskManager.GetResult(TaskID);
end;

function TAsyncTaskService.CancelTask(TaskID: Int64): Boolean;
begin
  Result := TaskManager.Cancel(TaskID);
end;

function TAsyncTaskService.ListActiveTasks: TInt64DynArray;
begin
  Result := TaskManager.ListActive;
end;

end.
