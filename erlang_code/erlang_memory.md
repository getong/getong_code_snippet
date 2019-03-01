# erlang memory
We can get the memory from the `erlang:memory` function.

```
1> erlang:memory(total).
12716856
2> erlang:memory(total) / 1024 / 1024.
12.171615600585938
3> erlang:memory(system) / 1024 / 1024.
8.389884948730469
4> erlang:memory(processes) / 1024 / 1024.
4.0694732666015625
5> erlang:memory() .
[{total,13097392},
{processes,4297656},
{processes_used,4297432},
{system,8799736},
{atom,202505},
{atom_used,189384},
{binary,58944},
{code,4092041},
{ets,274000}]
```
If the process's memory grows very quickly, the reason might be the message overload. The tcp stream data, and internal state not set properly, and single process bottleneck, all these might cause the memory increasing very high.

## get the top 10 most use memory processes

```
lists:sublist(lists:reverse(lists:keysort(2, lists:foldl(fun(Pid, Acc) -> case process_info(Pid, memory) of {memory, M} -> [{Pid, M} | Acc]; _ -> Acc end end, [], processes()))), 10).
```

## momory allocator

```
[{{A, N}, Data} || A <- [temp_alloc, eheap_alloc, binary_alloc, ets_alloc, driver_alloc, sl_alloc, ll_alloc, fix_alloc, std_alloc], {instance, N, Data} <- erlang:system_info({allocator,A})].
```
copy from [Memory constantly increasing - possibly fragmentation?](https://groups.google.com/forum/#!topic/rabbitmq-users/ALeIZ6VXJfc)
