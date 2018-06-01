# erlang singleton

## ns_server singleton

``` erlang
start_singleton(Module, Name, Args, Opts) ->
    case Module:start_link({global, Name}, Name, Args, Opts) of
        {error, {already_started, Pid}} ->
            ?log_debug("start_singleton(~p, ~p, ~p, ~p):"
                       " monitoring ~p from ~p",
                       [Module, Name, Args, Opts, Pid, node()]),
            {ok, spawn_link(fun () ->
                                    misc:wait_for_process(Pid, infinity),
                                    ?log_info("~p saw ~p exit (was pid ~p).",
                                              [self(), Name, Pid])
                            end)};
        {ok, Pid} = X ->
            ?log_debug("start_singleton(~p, ~p, ~p, ~p):"
                       " started as ~p on ~p~n",
                       [Module, Name, Args, Opts, Pid, node()]),
            X;
        X -> X
    end.
```
copy from ns_server misc.erl
It use the global name as a singleton in erlang cluster.
