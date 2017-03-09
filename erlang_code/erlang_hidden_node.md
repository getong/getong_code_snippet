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
