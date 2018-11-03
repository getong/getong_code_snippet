# lager

## rotate_log

``` erlang
%% @spec () -> ok
rotate_log() ->
    catch lager_crash_log ! rotate,
    lists:foreach(
      fun({lager_file_backend, File}) ->
              whereis(lager_event) ! {rotate, File};
         (_) ->
              ok
      end, gen_event:which_handlers(lager_event)).
```
copy from `ejabberd_logger.erl`. In case of `lager_crash_log` not run, adds `catch`.

## erlang 21 introduces logger module, which is inspired by lager partly.
The error_logger is deprecated by default in erlang 21, and lager enables error_logger by default.

## erlang 21 introduces logger, which can be replaced lager
The [emqx](https://github.com/emqx/emqx) just use logger to replace lager.
