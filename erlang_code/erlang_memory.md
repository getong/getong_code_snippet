# erlang memory
We can get the memory from the `erlang:memory` function.

``` erlang
erlang:memory(Type :: memory_type()) -> integer() >= 0
erlang:memory(TypeList :: [memory_type()]) ->
                 [{memory_type(), integer() >= 0}]
Types
memory_type() =
    total | processes | processes_used | system | atom |
    atom_used | binary | code | ets
```
example
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
Be default, there will be one instance of each allocators per scheduler.
recon plays a very important role in debugging. The script analyses the erl_crash.dump very well.
The comman memory leak is eheap_allocator problem, the main reason for that is the message in the process grows a lot, and even that the tcp binary data do not send to the client in a short time.
When debugging this problem, just adding some process time by time, and check the new processes. For example:

``` shell
1> OldProcesses = processes().
%% add quite few processes
2> NewProcesses = processes() -- OldProcesses.
%% check the current function of the process:
3> process_info(Pid, current_function).
%% check the message
4> process_info(Pid, messages).
```
process_info/1 or process_info/2 function are the key weapon we can use in the production.
