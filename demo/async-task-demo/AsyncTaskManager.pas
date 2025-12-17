/// Background Task Manager for mORMot 2
// - manages background task execution with thread pool
unit AsyncTaskManager;

interface

{$I mormot.defines.inc}

uses
  SysUtils,
  Classes,
  mormot.core.base,
  mormot.core.data,
  mormot.core.text,
  mormot.core.unicode,
  mormot.core.datetime,
  mormot.core.json,
  mormot.core.os,
  mormot.core.threads,
  mormot.core.log,
  AsyncTaskInterfaces;

type
  /// Internal task record
  PBackgroundTask = ^TBackgroundTask;
  TBackgroundTask = record
    TaskID: Int64;
    TaskName: RawUtf8;
    Status: TTaskStatus;
    Progress: Integer;
    DurationSeconds: Integer;
    ResultJson: RawJson;
    ErrorMessage: RawUtf8;
    Cancelled: Boolean;
    CreatedTix: Int64;
    StartedTix: Int64;
  end;
  TBackgroundTaskDynArray = array of TBackgroundTask;

  /// Thread-safe task manager
  TAsyncTaskManager = class
  private
    fTasks: TBackgroundTaskDynArray;
    fTaskCount: Integer;
    fNextTaskID: Int64;
    fLock: TRTLCriticalSection;
    function FindTask(TaskID: Int64): PBackgroundTask;
  public
    constructor Create;
    destructor Destroy; override;
    /// Create and queue a new task
    function CreateTask(DurationSeconds: Integer;
      const TaskName: RawUtf8): Int64;
    /// Get task result
    function GetResult(TaskID: Int64): TTaskResult;
    /// Cancel a task
    function Cancel(TaskID: Int64): Boolean;
    /// List all active tasks
    function ListActive: TInt64DynArray;
    /// Clean up completed tasks older than specified seconds
    procedure Cleanup(MaxAgeSeconds: Integer);
  end;

var
  /// Global task manager instance
  TaskManager: TAsyncTaskManager;

implementation

type
  /// Worker thread for executing tasks
  TTaskWorkerThread = class(TThread)
  private
    fTask: PBackgroundTask;
    fManager: TAsyncTaskManager;
  protected
    procedure Execute; override;
  public
    constructor Create(AManager: TAsyncTaskManager; ATask: PBackgroundTask);
  end;

{ TTaskWorkerThread }

constructor TTaskWorkerThread.Create(AManager: TAsyncTaskManager;
  ATask: PBackgroundTask);
begin
  fManager := AManager;
  fTask := ATask;
  FreeOnTerminate := True;
  inherited Create(False);
end;

procedure TTaskWorkerThread.Execute;
var
  startTix, elapsed, duration: Int64;
begin
  fTask^.Status := tsRunning;
  fTask^.StartedTix := GetTickCount64;
  startTix := fTask^.StartedTix;
  duration := fTask^.DurationSeconds * 1000;
  try
    // Simulate long-running work with progress updates
    while not fTask^.Cancelled do
    begin
      elapsed := GetTickCount64 - startTix;
      if elapsed >= duration then
        Break;
      // Update progress (0-100)
      fTask^.Progress := (elapsed * 100) div duration;
      Sleep(100); // Check every 100ms
    end;
    if fTask^.Cancelled then
    begin
      fTask^.Status := tsCancelled;
      fTask^.ErrorMessage := 'Task was cancelled by user';
    end
    else
    begin
      fTask^.Progress := 100;
      fTask^.Status := tsCompleted;
      // Simulate result
      fTask^.ResultJson := JsonEncode([
        'taskId', fTask^.TaskID,
        'taskName', fTask^.TaskName,
        'duration', fTask^.DurationSeconds,
        'completedAt', DateTimeToIso8601(Now, True)
      ]);
    end;
  except
    on E: Exception do
    begin
      fTask^.Status := tsFailed;
      StringToUtf8(E.Message, fTask^.ErrorMessage);
    end;
  end;
end;

{ TAsyncTaskManager }

constructor TAsyncTaskManager.Create;
begin
  inherited Create;
  InitializeCriticalSection(fLock);
  fNextTaskID := 1;
  fTaskCount := 0;
  SetLength(fTasks, 64); // Pre-allocate
end;

destructor TAsyncTaskManager.Destroy;
var
  i: Integer;
begin
  // Cancel all running tasks
  EnterCriticalSection(fLock);
  try
    for i := 0 to fTaskCount - 1 do
      fTasks[i].Cancelled := True;
  finally
    LeaveCriticalSection(fLock);
  end;
  Sleep(200); // Give threads time to finish
  DeleteCriticalSection(fLock);
  inherited;
end;

function TAsyncTaskManager.FindTask(TaskID: Int64): PBackgroundTask;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to fTaskCount - 1 do
    if fTasks[i].TaskID = TaskID then
    begin
      Result := @fTasks[i];
      Exit;
    end;
end;

function TAsyncTaskManager.CreateTask(DurationSeconds: Integer;
  const TaskName: RawUtf8): Int64;
var
  task: PBackgroundTask;
  worker: TTaskWorkerThread;
begin
  EnterCriticalSection(fLock);
  try
    // Grow array if needed
    if fTaskCount >= Length(fTasks) then
      SetLength(fTasks, fTaskCount + 64);
    task := @fTasks[fTaskCount];
    Inc(fTaskCount);
    // Initialize task
    task^.TaskID := fNextTaskID;
    Inc(fNextTaskID);
    Result := task^.TaskID;
    task^.TaskName := TaskName;
    task^.Status := tsQueued;
    task^.Progress := 0;
    task^.DurationSeconds := DurationSeconds;
    task^.ResultJson := '';
    task^.ErrorMessage := '';
    task^.Cancelled := False;
    task^.CreatedTix := GetTickCount64;
    task^.StartedTix := 0;
  finally
    LeaveCriticalSection(fLock);
  end;
  // Start worker thread
  worker := TTaskWorkerThread.Create(Self, task);
end;

function TAsyncTaskManager.GetResult(TaskID: Int64): TTaskResult;
var
  task: PBackgroundTask;
begin
  FillCharFast(Result, SizeOf(Result), 0);
  Result.TaskID := TaskID;
  EnterCriticalSection(fLock);
  try
    task := FindTask(TaskID);
    if task = nil then
    begin
      Result.Status := tsFailed;
      Result.ErrorMessage := 'Task not found';
      Exit;
    end;
    Result.Status := task^.Status;
    Result.Progress := task^.Progress;
    Result.ResultJson := task^.ResultJson;
    Result.ErrorMessage := task^.ErrorMessage;
  finally
    LeaveCriticalSection(fLock);
  end;
end;

function TAsyncTaskManager.Cancel(TaskID: Int64): Boolean;
var
  task: PBackgroundTask;
begin
  Result := False;
  EnterCriticalSection(fLock);
  try
    task := FindTask(TaskID);
    if (task <> nil) and (task^.Status in [tsQueued, tsRunning]) then
    begin
      task^.Cancelled := True;
      Result := True;
    end;
  finally
    LeaveCriticalSection(fLock);
  end;
end;

function TAsyncTaskManager.ListActive: TInt64DynArray;
var
  i, n: Integer;
begin
  Result := nil;
  EnterCriticalSection(fLock);
  try
    n := 0;
    SetLength(Result, fTaskCount);
    for i := 0 to fTaskCount - 1 do
      if fTasks[i].Status in [tsQueued, tsRunning] then
      begin
        Result[n] := fTasks[i].TaskID;
        Inc(n);
      end;
    SetLength(Result, n);
  finally
    LeaveCriticalSection(fLock);
  end;
end;

procedure TAsyncTaskManager.Cleanup(MaxAgeSeconds: Integer);
var
  i, j: Integer;
  threshold: Int64;
begin
  threshold := GetTickCount64 - (MaxAgeSeconds * 1000);
  EnterCriticalSection(fLock);
  try
    j := 0;
    for i := 0 to fTaskCount - 1 do
    begin
      // Keep if still active or not old enough
      if (fTasks[i].Status in [tsQueued, tsRunning]) or
         (fTasks[i].CreatedTix >= threshold) then
      begin
        if i <> j then
          fTasks[j] := fTasks[i];
        Inc(j);
      end;
    end;
    fTaskCount := j;
  finally
    LeaveCriticalSection(fLock);
  end;
end;

initialization
  TaskManager := TAsyncTaskManager.Create;

finalization
  FreeAndNil(TaskManager);

end.
