#erlang port
## get all ports

```
erlang:ports().
%% length of all ports
length(erlang:ports()).
```

## is port

```
[H|_] = erlang:ports(),
erlang:is_port(H).
```
