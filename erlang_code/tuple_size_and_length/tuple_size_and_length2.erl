-module(tuple_size_and_length2).

%% API
-export([start/0]).

%%%===================================================================
%%% API
%%%===================================================================
start() ->
	ANum = rand:uniform(100),
	BNum = rand:uniform(1000),
	CNum = rand:uniform(10000),
	DNum = rand:uniform(100000),
	%%ENum = rand:uniform(1000000),
	{TupleA, ListA}  = tuple_and_list(ANum),
	{TupleB, ListB}  = tuple_and_list(BNum),
	{TupleC, ListC}  = tuple_and_list(CNum),
	{TupleD, ListD}  = tuple_and_list(DNum),
	%%{TupleE, ListE}  = tuple_and_list(ENum),
	[tuple_list_size_compute(TupleA, ListA, ANum) || _Id <- lists:seq(1, 10)],
	[tuple_list_size_compute(TupleB, ListB, BNum) || _Id <- lists:seq(1, 10)],
	[tuple_list_size_compute(TupleC, ListC, CNum) || _Id <- lists:seq(1, 10)],
	[tuple_list_size_compute(TupleD, ListD, DNum) || _Id <- lists:seq(1, 10)].
	%%tuple_list_size_compute(TupleE, ListE, ENum) .


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
tuple_list_size_compute(Tuple, List, Num) ->
	{_, R1} = process_info(self(), reductions),

	TupleTime = timer:tc(erlang, tuple_size, [Tuple]),
	{_, R2} = process_info(self(), reductions),

	ListTime = timer:tc(erlang, length, [List]),
	{_, R3} = process_info(self(), reductions),

	io:format("Num:~p, tuple size compute:~p, list length compute:~p, TupleTime:~p, ListTime:~p~n", [Num, R2 - R1, R3 - R2, TupleTime, ListTime]).

tuple_and_list(Num) ->
	Tuple = lists:foldl(fun(_, Acc) ->
								erlang:append_element(Acc, a)
						end, {}, lists:seq(1, Num)),
	List = lists:foldl(fun(_, Acc) ->
							   [a | Acc]
					   end, [], lists:seq(1,Num)),
	{Tuple, List}.
