# riak_ensemble_peer_worker

## communicate with ets table

``` erlang
pause_workers(_Workers, ETS) ->
    ets:insert(ETS, {paused, true}),
        ok.

unpause_workers(Workers, ETS) ->
    ets:delete(ETS, paused),
    _ = [Pid ! unpause || Pid <- Workers],
    ok.
```

## only run Fun, not know of what the Fun is

``` erlang
loop(ETS) ->
    receive
        {async, Fun} ->
            maybe_pause(ETS),
            Fun();
        {'DOWN', _, _, _, _} ->
            exit(normal);
        _ ->
            ok
    end,
    ?MODULE:loop(ETS).
```
