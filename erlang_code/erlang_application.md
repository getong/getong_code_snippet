# erlang application

## determine what application has loaded

```
application:which_applications(),
```


## application key

``` erlang
1> application:get_all_key().
2> application:get_all_key(AppName).
3> application:get_key(Key).
4> application:get_key(AppName, Key).
```

The `Key` is defined on the `*.app.src` or "*.app" file, and `env` is one key of the all keys.
The `sys.config` is all the `env` variables collection.

``` erlang
5> E1 = application:get_all_env(AppName).
6> {ok, E2} = application:get_key(AppName, env).
7> E1 == E2.
true
```

## prep_stop

``` erlang
%% Prepare the application for termination.
%% This function is called when an application is about to be stopped,
%% before shutting down the processes of the application.
prep_stop(State) ->
    ejabberd_listener:stop_listeners(),
    ejabberd_sm:stop(),
    gen_mod:stop_modules(),
    State.
```
copy from ejabberd_app.erl
The prep_stop/1 function is very powerfull.

## The `State` in the start/2 and stop/1

``` erlang
Module:start(StartType, StartArgs) -> {ok, Pid} | {ok, Pid, State} | {error, Reason}

The function is to return {ok,Pid} or {ok,Pid,State}, where Pid is the pid of the top supervisor and State is any term. If
omitted, State defaults to []. If the application is stopped later, State is passed to Module:prep_stop/1.

Module:stop(State)
State is the return value of Module:prep_stop/1, if such a function exists. Otherwise State is taken from the return value of
Module:start/2.
```
An example is the lager:

``` erlang
start(_StartType, _StartArgs) ->
    {ok, Pid} = lager_sup:start_link(),
    SavedHandlers = boot(),
    _ = boot('__all_extra'),
    _ = boot('__traces'),
    clean_up_config_checks(),
    {ok, Pid, SavedHandlers}.

boot() ->
    %% Handle the default sink.
    determine_async_behavior(?DEFAULT_SINK,
                             application:get_env(lager, async_threshold, undefined),
                             application:get_env(lager, async_threshold_window, undefined)),

    _ = maybe_install_sink_killer(?DEFAULT_SINK, application:get_env(lager, killer_hwm, undefined),
                                  application:get_env(lager, killer_reinstall_after, undefined)),

    start_handlers(?DEFAULT_SINK,
                   application:get_env(lager, handlers, ?DEFAULT_HANDLER_CONF)),

    lager:update_loglevel_config(?DEFAULT_SINK),

    SavedHandlers = start_error_logger_handler(
                      application:get_env(lager, error_logger_redirect, true),
                      interpret_hwm(application:get_env(lager, error_logger_hwm, 0)),
                      application:get_env(lager, error_logger_whitelist, [])
                     ),

    SavedHandlers.

stop(Handlers) ->
    lists:foreach(fun(Handler) ->
          error_logger:add_report_handler(Handler)
      end, Handlers).
```
If start the lager application, it will stop some error_logger handlers, and if stop the lager application, it will start the stopped handlers.
