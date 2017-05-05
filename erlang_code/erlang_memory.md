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
