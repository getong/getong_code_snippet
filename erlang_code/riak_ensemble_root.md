# riak_ensemble root
create ensemble first set the `cluster_state` in the root ensemble,
then gossip operation will get the `cluster_state` back and gossip the root ensemble.
It finally call the code below:
```
%%% copy from riak_ensemble_root.erl

%% This function implements a non-blocking w/ backpressure approach to sending
%% a message to the ensemble manager. Directly calling _manager:gossip would
%% block the root leader. Changing _manager:gossip to use a cast would provide
%% no backpressure. Instead, the leader spawns a singleton process that blocks
%% on the call. As long as the singleton helper is still alive, no new process
%% will be spawned.
maybe_async_gossip(State) ->
    Async = erlang:get(async_gossip_pid),
    CurrentAsync = is_pid(Async) andalso is_process_alive(Async),
    case CurrentAsync of
        true ->
            ok;
        false ->
            Async2 = spawn(fun() ->
                                   riak_ensemble_manager:gossip(State)
                           end),
            erlang:put(async_gossip_pid, Async2),
            ok
    end.
```
