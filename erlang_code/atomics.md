# erlang atomics module

>> Atomics: atomic operations towards mutable atomic variables.
>> Counters: built on atomics
>> No SW level locking, very efficient for concurrent access
>> significantly more efficient and scalable than ets:update_counter(Tab, Key, UpdateOp) -> Result
since 21.2
``` erlang
Cref = counters:new(10, []).
counters:add(Cref, 1, 1).
counters:get(Cref, 1).
```
