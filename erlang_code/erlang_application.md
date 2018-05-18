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
