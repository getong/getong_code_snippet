# erlang OS environment

## os:getenv/1 works in linux, windows probably not

```
%% in linux works, windows not
1> os:getenv("HOME").


```
## init:get_argument/1 works on most OS, but only a few arguments.

```
1>init:get_argument(home).
```

## os signal handling

``` erlang
os:set_signal(Signal, Option) -> ok
Types
Signal =
    sighup |
    sigquit |
    sigabrt |
    sigalrm |
    sigterm |
    sigusr1 |
    sigusr2 |
    sigchld |
    sigstop |
    sigtstp
Option = default | handle | ignore

...

handle
This signal will notify erl_signal_server when it is received by the Erlang runtime system.

```

## erl_signal_server is gen_event, and can swap its handler

``` erlang
handle_event(sigterm, S) ->
    error_logger:info_msg("SIGTERM received - shutting down~n"),
    ok = init:stop(),
    {ok, S};
```
copy from erl_signal_handler.erl, which is from erlang/otp.

``` erlang
 ok = gen_event:swap_sup_handler(
        erl_signal_server,
        {erl_signal_handler, []},
        {k8s_signal_handler, [Table, Delay, Test]}),
```
copy from [k8s_traffic_plug](https://github.com/Financial-Times/k8s_traffic_plug)
