# erlang init

## stop node

``` erlang
init:stop().
```

## restart node

``` erlang
init:restart().
```

## get sys.config file location

``` shell
init:get_argument(config)
```

## docker stop erlang_container_id

``` erlang
erts: A received SIGTERM signal to beam will generate a 'stop' message to the init process and terminate the Erlang VM nicely. This is equivalent to calling init:stop/0.
```
see [Erlang/OTP 19.3 has been released](http://www.erlang.org/news/110)

In erlang application module, write the stop function inside the stop/1 funtion.

``` erlang
stop(_State) ->
    lager:info("Stop recieved."),
    erlang:display("Stop recieved."),
    ok.
```
see [Erlang: does the application behavior trap SIGTERM?](https://stackoverflow.com/questions/42912781/erlang-does-the-application-behavior-trap-sigterm)

## use halt/1 in erlang cmd operation

``` erlang
fail() ->
    io:format("fail", []),
    halt(1).
```

## ct_slave
the `stop` function in ct_slave module is just use the `init:stop/1` function to ensure the node is not connected.
