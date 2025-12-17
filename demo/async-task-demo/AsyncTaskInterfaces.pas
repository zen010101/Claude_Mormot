/// Long-Running Task Interfaces for mORMot 2
// - demonstrates asynchronous pattern for long-running operations
unit AsyncTaskInterfaces;

interface

{$I mormot.defines.inc}

uses
  mormot.core.base,
  mormot.core.data,
  mormot.core.json,
  mormot.core.interfaces;

type
  /// Status of a background task
  TTaskStatus = (
    tsQueued,
    tsRunning,
    tsCompleted,
    tsFailed,
    tsCancelled
  );

  /// Task result record - returned by GetTaskResult
  TTaskResult = packed record
    TaskID: Int64;
    Status: TTaskStatus;
    Progress: Integer;      // 0-100
    ResultJson: RawJson;
    ErrorMessage: RawUtf8;
  end;

  /// Interface for long-running task operations
  // - uses polling pattern to keep session alive during long operations
  IAsyncTaskService = interface(IInvokable)
    ['{A1B2C3D4-E5F6-4789-ABCD-EF0123456789}']
    /// Start a simulated long-running task
    // - DurationSeconds: how long the task should run (simulated)
    // - TaskName: optional name for the task
    // - Returns: TaskID for tracking
    function StartTask(DurationSeconds: Integer;
      const TaskName: RawUtf8): Int64;
    /// Check task status and get result if completed
    // - This call keeps the session alive (updates LastAccessTix10)
    function GetTaskResult(TaskID: Int64): TTaskResult;
    /// Cancel a running task
    function CancelTask(TaskID: Int64): Boolean;
    /// List all active task IDs
    function ListActiveTasks: TInt64DynArray;
  end;

implementation

initialization
  TInterfaceFactory.RegisterInterfaces([
    TypeInfo(IAsyncTaskService)
  ]);

end.
