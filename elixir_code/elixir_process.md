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

## elixir Registry
```
Registry.start_link/2
Registry.register/3
Registry.lookup/2
```
If the process registered in the registry dies, it will automaticly deleted in the Registry

## max concurrency
The Task.async_stream/4 functon can sets the option :max_concurrency, to set the maximum number of tasks to run at the same time. Defaults to System.schedulers_online/0.

## Registry
Registry all keys
``` elixir
defmodule MyApp.Sensor.Registry do
  @doc """
  Lists all the registered sensors from the registry.
  """
  @spec list_sensors!(module) :: [{String.t(), pid, any}]
  def list_sensors!(registry \\ __MODULE__) do
    try do
      # The args for `lookup_element` are taken from the private function
      # `info/1` from the `Registry` module.
      {_kind, _partitions, table, _pid_ets, _} =
        :ets.lookup_element(registry, -1, 2)

      # select_all is the result of `:ets.fun2ms(&(&1))` which works from iex
      # but not when compiled for reasons beyond my comprehension
      select_all = [{:"$1", [], [:"$1"]}]
      :ets.select(table, select_all)
    catch
      :error, :badarg ->
        raise ArgumentError, "#{inspect(registry)} is not running"
    end
  end
end
```
copy from [How to get keys (pids) of registered via Register children](https://stackoverflow.com/questions/42086249/how-to-get-keys-pids-of-registered-via-register-children)
