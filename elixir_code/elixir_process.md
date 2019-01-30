# elixir process

## proces info
``` elixir
pid = spawn(fn -> Process.sleep(:infinity))

Porcess.info(pid)[:message_queue_len]
Process.info(pid)
```

## elixir update process version

``` elixir
:sys.suspend(ProcName)
c("module_name.ex")
## the change_code/4 function will call the code_change/3 in the gen_server or something like modules.
:sys.change_code(ProcName, module_name, "0", [])
:sys.resume(ProcName)
```
This is a example from <<Programming Elixir>>


## BEAM, scheduler, process

```
BEAM is a single OS process.
Scheduler is an OS thread responsible for executing multiple processes.
Erlang process is a unit of concurrent execution.
BEAM uses multiple schedulers to parallelize the work ovr available CPU cores.
```
