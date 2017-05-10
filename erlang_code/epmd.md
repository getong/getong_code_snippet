# epmd

## node() and net_adm:names()

``` erlang
$erl
1> node().
test@hostname
2> net_adm:names().
{ok, [{"test", 48299}, {"erlang", 48298}]}
```

net_adm:names() returns the node names and the port used to connect to other erlang nodes
