# erlang kv struct

## benchmark
see [A benchmark of different Erlang KV structures by Sergey Prokhorov (@seriyps)](http://tryerl.seriyps.ru/#id=f9f3)

``` erlang
% -*- coding: utf8 -*-
%% Benchmarking code samples.
%% Each benchmark is launched in it's own process to eliminate GC influence.
-module(benchmark).
-export([main/0]).


main() ->
    % {Test name, TestFun1, TestFun2, Constructor}
    % Constructor runtime isn't counted
    io:format("~n=== Find ===~n"),
    test(find_set()),
    io:format("~n=== Insert ===~n"),
    test(ins_set()).

find_set() ->
    [
        {"proplist", fun find_proplist/1, fun(LK) -> LK end},
        {"lists",    fun find_lists/1,    fun(LK) -> LK end},
        {"orddict",  fun find_orddict/1,  fun({List, Key}) -> {orddict:from_list(List), Key} end},
        {"dict",     fun find_dict/1,     fun({List, Key}) -> {dict:from_list(List), Key} end},
        {"gb_trees", fun find_gb_trees/1, fun({List, Key}) -> {gb_trees:from_orddict(orddict:from_list(List)), Key} end},
        {"maps",     fun find_maps/1,     fun({List, Key}) -> {maps:from_list(List), Key} end}
    ].

ins_set() ->
    [
        {"proplist", fun ins_proplist/1, fun(LK) -> LK end},
        {"lists",    fun ins_lists/1,    fun(LK) -> LK end},
        {"orddict",  fun ins_orddict/1,  fun({List, Key}) -> {orddict:from_list(List), Key} end},
        {"dict",     fun ins_dict/1,     fun({List, Key}) -> {dict:from_list(List), Key} end},
        {"gb_trees", fun ins_gb_trees/1, fun({List, Key}) -> {gb_trees:from_orddict(orddict:from_list(List)), Key} end},
        {"maps",     fun ins_maps/1,     fun({List, Key}) -> {maps:from_list(List), Key} end}
    ].

test(Probes) ->
    [begin
        NIter = max(trunc(200000 / Size), 1),
        io:format("~nSize: ~p; NIter: ~p~n", [Size, NIter]),
        Data = [{integer_to_binary(V), V} || V <- lists:seq(1, Size)],
        Key = integer_to_binary(Size div 2),
        run(Probes, {Data, Key}, NIter)
    end || Size <- [5, 10, 50, 100, 1000, 10000, 70000]].

% Put your benchmarked code below:

find_proplist({PList, Key}) ->
    proplists:get_value(Key, PList).

find_lists({List, Key}) ->
    case lists:keyfind(Key, 1, List) of
        false -> undefined;
        {Key, Val} -> Val
    end.

find_orddict({Orddict, Key}) ->
    case orddict:find(Key, Orddict) of
        error -> undefined;
        {ok, Val} -> Val
    end.

find_dict({Dict, Key}) ->
    case dict:find(Key, Dict) of
        error -> undefined;
        {ok, Val} -> Val
    end.

find_gb_trees({GbTree, Key}) ->
    case gb_trees:lookup(Key, GbTree) of
        none -> undefined;
        {value, Val} -> Val
    end.


find_maps({Map, Key}) ->
    maps:get(Key, Map, undefined).


ins_proplist({PList, Key}) ->
    [{Key, 1} | PList].

ins_lists({List, Key}) ->
    [{Key, 1} | List].

ins_orddict({Orddict, Key}) ->
    orddict:store(Key, 1, Orddict).

ins_dict({Dict, Key}) ->
    dict:store(Key, 1, Dict).

ins_gb_trees({GbTree, Key}) ->
    gb_trees:enter(Key, 1, GbTree).

ins_maps({Map, Key}) ->
    maps:put(Key, 1, Map).

% INTERNAL:
run(Probes, Args, NIter) ->
    io:format("Name           |Time / per iter (us) ||Last Memory |Reductions |N GCs     |Last result~n"),
    io:format("---------------+---------------------++------------+-----------+----------+----------------------~n"),
    run1(Probes, Args, NIter).

run1([{Name, Fun, Init} | Probes], Args, NIter) when NIter > 0 ->
    erlang:garbage_collect(self()),
    Master = self(),
    % run each sample in it's own process, so results are less skewed by GC.
    Args1 = Init(Args),
    Pid = spawn_link(fun() ->
        {Time, Result} = timer:tc(fun () -> repeat(Fun, Args1, NIter) end),
        Master ! {self(), Time, Result, process_info(self(), [memory, reductions, garbage_collection])}
    end),
    receive
        {Pid, Time, Result, [{memory, Mem}, {reductions, Red}, {garbage_collection, GC}]} ->
            MinorGCs = proplists:get_value(minor_gcs, GC),
            io:format("~-15s|~10s /~9s||~12s|~11s|~10s|~180P~n",
                     [Name, integer_to_list(Time), integer_to_list(round(Time / NIter)), integer_to_list(Mem),
                      integer_to_list(Red), integer_to_list(MinorGCs), Result, 4])
    end,
    run1(Probes, Args, NIter);
run1([], _, _) -> ok.

repeat(Fun, Args, 1) ->
    Fun(Args);
repeat(Fun, Args, N) ->
    Fun(Args),
    repeat(Fun, Args, N - 1).

```
run it:

``` shell
erlc benchmark.erl
erl -s benchmark main -s init stop > benchmark.out
```

the `maps` is better than `dict` type.
