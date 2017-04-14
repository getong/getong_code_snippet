#erlang hidden node
## erl start hidden node

```
erl -hidden -sname a

```

## escript start hidden node

```
#!/usr/bin/env escript
%% -*- erlang -*-
%%! -hidden
```

## net_kernel:hidden_connect_node/1, no docs, but exported.

```
net_kernel:connect_node/1  %% connect to node, not hidden
net_kernel:hidden_connect_node(Node) %% connect to node, hidden
```

## set_net_ticktime

``` erlang
net_kernel:set_net_ticktime/1, net_kernel:set_net_ticktime/2 are used to change the node ticktime.
```
