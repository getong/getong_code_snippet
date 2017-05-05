# list operation

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

## lists:ukeymerge/3, use `A` lists first, if not found, then use the  `B` list.

``` erlang
merge(TabDef, CustomDef) ->
    {CustomKeys, _} = lists:unzip(CustomDef),
    CleanDef = lists:foldl(
		fun(Elem, Acc) ->
		    case lists:member(Elem, ?STORAGE_TYPES) of
			true ->
			    lists:foldl(
			      fun(Key, CleanAcc) ->
				      lists:keydelete(Key, 1, CleanAcc)
			      end, Acc, ?STORAGE_TYPES);
			false ->
			    Acc
		    end
		end, TabDef, CustomKeys),
    lists:ukeymerge(1,
		   lists:ukeysort(1, CustomDef),
		   lists:ukeysort(1, CleanDef)).
```
The `CustomDef` is override the `CleanDef`. Good practice.
Example:

``` erlang
erl>
1> A = [{a, 1}, {b, 2}, {c, 3}].
[{a,1},{b,2},{c,3}]
2> B = [{a, 2}, {b, 3}, {c, 4}].
[{a,2},{b,3},{c,4}]
3> lists:ukeymerge(1, A, B).
[{a,1},{b,2},{c,3}]
4> lists:ukeymerge(1,  B, A).
[{a,2},{b,3},{c,4}]
```
