#list operation

```
%% copy from riak_ensemble_peer.erl
remove_watcher(Pid, WatcherList) ->
    case lists:keytake(Pid, 1, WatcherList) of
        false ->
            not_found;
        {value, {_Pid, MRef}, NewWatcherList} ->
            {MRef, NewWatcherList}
    end.

%% copy from riak_ensemble_msg.erl
-spec find_valid([peer_reply()]) -> {[peer_reply()], [peer_nack()]}.
find_valid(Replies) ->
    {Valid, Nacks} = lists:partition(fun({_, nack}) ->
                                             false;
                                        (_) ->
                                             true
                                     end, Replies),
    {Valid, Nacks}.
```

%% find a element from record list

```

-record(fact, {
	a,
	b,
	c}).

L = [{fact, 1, 2,3},{fact, 1, 2,4},{fact, 1, 2,5},{fact, 1, 2,7}].
lists:keyfind(1, #fact.a, L).

```
