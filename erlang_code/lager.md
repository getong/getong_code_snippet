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
