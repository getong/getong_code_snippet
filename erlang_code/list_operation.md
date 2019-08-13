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

## For proplists, sometimes it is better to use lists module

``` erlang
%% proplists:get_value/3
-spec get_value(Key, List, Default) -> term() when
      Key :: term(),
      List :: [term()],
      Default :: term().

get_value(Key, [P | Ps], Default) ->
    if is_atom(P), P =:= Key ->
	    true;
       tuple_size(P) >= 1, element(1, P) =:= Key ->
	    case P of
		{_, Value} ->
		    Value;
		_ ->
		    %% Don</code>t continue the search!
		    Default
	    end;
       true ->
	    get_value(Key, Ps, Default)
    end;
get_value(_Key, [], Default) ->
    Default.

%% lists:keyfind/3
%% Shadowed by erl_bif_types: lists:keyfind/3
-spec keyfind(Key, N, TupleList) -> Tuple | false when
      Key :: term(),
      N :: pos_integer(),
      TupleList :: [Tuple],
      Tuple :: tuple().

keyfind(_, _, _) ->
    erlang:nif_error(undef).
```
lists:keyfind/3 is nif implementation, better in performance.
For [{Key, Value}] proplists, it is better to use lists:keyfind/3 than proplists:get_value/3.

## format a number with padding in Erlang

``` erlang
string:right(integer_to_list(4), 4, $0).
```
see [How to format a number with padding in Erlang](https://stackoverflow.com/questions/1251869/how-to-format-a-number-with-padding-in-erlang)

## delete duplicate element in the list

``` erlang
 Set = sets:from_list(List),
 sets:to_list(Set).
```

## index_of

``` erlang
index_of(Item, List) -> index_of(Item, List, 1).

index_of(_, [], _)  -> not_found;
index_of(Item, [Item|_], Index) -> Index;
index_of(Item, [_|Tl], Index) -> index_of(Item, Tl, Index+1).
```
copy from [Erlang lists:index_of function?](https://stackoverflow.com/questions/1459152/erlang-listsindex-of-function)

## Breaking out of lists:foreach "loop"
use throw ... catch to quickly break lists operation, no need to handle all elements of a list.

``` erlang
try lists:foreach(
        fun(1) ->
                throw(found_one);
           (X) ->
                io:format("~p~n", [X])
        end,
        [2, 4, 5, 1, 2, 5])
catch
    throw:found_one ->
        found_one
end.
```
copy from [Erlang : Breaking out of lists:foreach "loop"](https://stackoverflow.com/questions/1820241/erlang-breaking-out-of-listsforeach-loop)
also see [Easy way to break foldl](https://stackoverflow.com/questions/8412446/easy-way-to-break-foldl)

## ets foldl and foldr functions

``` erlang
foldl(F, Accu, T) ->
    ets:safe_fixtable(T, true),
    First = ets:first(T),
    try
        do_foldl(F, Accu, First, T)
    after
	ets:safe_fixtable(T, false)
    end.

do_foldl(F, Accu0, Key, T) ->
    case Key of
	'$end_of_table' ->
	    Accu0;
	_ ->
	    do_foldl(F,
		     lists:foldl(F, Accu0, ets:lookup(T, Key)),
		     ets:next(T, Key), T)
    end.

foldr(F, Accu, T) ->
    ets:safe_fixtable(T, true),
    Last = ets:last(T),
    try
        do_foldr(F, Accu, Last, T)
    after
        ets:safe_fixtable(T, false)
    end.

do_foldr(F, Accu0, Key, T) ->
    case Key of
	'$end_of_table' ->
	    Accu0;
	_ ->
	    do_foldr(F,
		     lists:foldr(F, Accu0, ets:lookup(T, Key)),
		     ets:prev(T, Key), T)
    end.
```
The foldl and foldr both use the `ets:saafe_fixtable/2` function to ensure the ets lock the table data.
The `ets:first/1` get the first data, and then use the `ets:next/2` function to get the next data.
The `ets:last/1` get the last data, and then use the `ets:prev/2` function to get the previous data.

## binary and list convert

list to binary
``` erlang
A = [130, 161, 98, 1, 161, 97, 1].
lists:foldr(fun(X, Acc) ->
                        case Acc of
                            <<>> ->
                                integer_to_binary(X);
                            _ ->
                                << (integer_to_binary(X))/binary, ",", Acc/binary>>
                        end
                end, <<>>, A).
%% <<"130,161,98,1,161,97,1">>
```
 binary to list
```erlang
A = <<"130,161,98,1,161,97,1">>.
lists:map(fun(E) -> binary_to_integer(E) end, binary:split(A, <<",">>, [global])).
%% [130,161,98,1,161,97,1]
```
