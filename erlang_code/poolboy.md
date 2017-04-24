#poolboy

## eredis_pool start args

``` erlang
PoolSpecs = lists:map(fun({Name, SizeArgs, WorkerArgs}) ->
        PoolArgs = [{name, {GlobalOrLocal, Name}},
                    {worker_module, eredis}] ++ SizeArgs,
        poolboy:child_spec(Name, PoolArgs, WorkerArgs)
    end, Pools),
```
As we can see here, The `poolboy:child_spec` uses three arguments, `Name` is the process pool name; the `PoolArgs` is the pool arguments, includes how to schedule the processes in the pool, the num of the pool, and if the pool is all used and then how to spawn new processes; the `WorkerArgs` is the arguments passed to the worker modules.
Very Straghtword.

## How to use poolboy

``` erlang
q(PoolName, Command, Timeout) ->
    poolboy:transaction(PoolName, fun(Worker) ->
                                          eredis:q(Worker, Command, Timeout)
                                  end).
```
Get one process of the poolname, and execute the fun in the process.
