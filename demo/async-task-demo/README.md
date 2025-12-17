# Async Task Demo - mORMot 2 Long-Running Operations

This demo demonstrates how to handle long-running operations in mORMot 2 services without triggering session timeouts.

## Problem

When using mORMot 2 services with `sicClientDriven` instance creation mode, the server has a `TimeoutSec` setting that automatically releases idle service instances. This becomes problematic when:

- Executing long-running database stored procedures (e.g., with FireDAC)
- Performing time-consuming calculations
- Waiting for external resources

The default workaround of setting `TimeoutSec := 0` globally affects all connections, which is not ideal.

**Forum Discussion:** https://synopse.info/forum/viewtopic.php?id=7449

## Solution: Polling Pattern

Instead of making a single long-running service call, this demo implements an asynchronous polling pattern:

1. **Client starts a task** - Returns immediately with a TaskID
2. **Client polls for status** - Each poll call keeps the session alive
3. **Server executes in background** - Using a worker thread pool
4. **Client retrieves result** - When task completes

Each `GetTaskResult()` call updates `LastAccessTix10` in `TServiceFactoryServer.RetrieveInstance`, preventing the session from being considered idle.

## Architecture

```
┌─────────────────┐         ┌─────────────────┐
│  AsyncTask      │  HTTP   │  AsyncTask      │
│  Client         │◄───────►│  Server         │
│                 │         │                 │
│  - StartTask    │         │  - Service Impl │
│  - Poll Status  │         │  - Task Manager │
│  - Get Result   │         │  - Worker Pool  │
└─────────────────┘         └─────────────────┘
```

## Files

| File | Description |
|------|-------------|
| `AsyncTaskInterfaces.pas` | Service interface and data types |
| `AsyncTaskManager.pas` | Thread-safe background task manager |
| `AsyncTaskServiceImpl.pas` | Service implementation |
| `AsyncTaskServer.dpr` | Server application |
| `AsyncTaskServer.lpi` | Lazarus project for server |
| `AsyncTaskClient.dpr` | Client application |
| `AsyncTaskClient.lpi` | Lazarus project for client |

## Building

### With Free Pascal / Lazarus

```bash
# Build server
lazbuild AsyncTaskServer.lpi

# Build client
lazbuild AsyncTaskClient.lpi
```

### With Delphi

Open the `.dpr` files in Delphi IDE and compile.

## Usage

1. **Start the server:**
   ```
   AsyncTaskServer.exe
   ```
   The server listens on port 8888 with a 60-second session timeout.

2. **Run the client:**
   ```
   AsyncTaskClient.exe
   ```

3. **Enter task duration** when prompted (1-60 seconds)

4. **Watch the polling** - The client polls every 500ms and displays progress

## Key Code Snippets

### Server: Service Registration with Timeout

```pascal
Server.ServiceDefine(TAsyncTaskService, [IAsyncTaskService], sicClientDriven);
(Server.Services.Index(0) as TServiceFactoryServer).TimeoutSec := 60;
```

### Client: Service Registration

```pascal
Client.ServiceDefine([IAsyncTaskService], sicClientDriven);
if not Client.Services.Resolve(IAsyncTaskService, Service) then
  // Handle error
```

### Polling Loop

```pascal
repeat
  SleepHiRes(500); // Poll every 500ms
  Result := Service.GetTaskResult(TaskID);
  // Display progress...
until Result.Status in [tsCompleted, tsFailed, tsCancelled];
```

## Why This Works

The `TimeoutSec` mechanism in mORMot 2 checks `LastAccessTix10` to determine if a session is idle. Each service method call updates this timestamp. By using polling instead of a single blocking call:

- The session remains active during long operations
- Server resources are properly managed
- Other connections still benefit from timeout protection
- No global configuration changes needed

## License

This demo is part of the mORMot 2 framework examples.
See https://github.com/synopse/mORMot2 for licensing information.
