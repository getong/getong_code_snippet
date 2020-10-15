-module(max_string).

%% API
-export([start/2]).

%%%===================================================================
%%% API
%%%===================================================================
%% get the most common substring
start(AString, BString) ->
    ALength = length(AString),
    BLength = length(BString),
    case ALength > BLength of
        true ->
            get_max_substring(BString, AString);
        false ->
            get_max_substring(AString, BString)
    end.


get_max_substring(AString, BString) ->
    {_MaxInxdex, MaxLength, MaxString} =
    lists:foldl(fun(Char, {Index, Acc, StringAcc}) ->
                        {NewAcc, NewStringAcc} = start_compare_string(Char,Index, Acc, StringAcc, AString, BString),
                        {Index+1, NewAcc, NewStringAcc}
                end, {0, 0, []}, AString),
    {MaxLength, MaxString}.


start_compare_string(Char,Index, Acc, StringAcc, AString, BString) ->
    case is_in_string(Char, BString, 0) of
        false ->
            {Acc, StringAcc};
        BIndex ->
            {_, ASubList} = lists:split(Index, AString),
            {_, BSubList} = lists:split(BIndex, BString),
            {TempLength, TempStringAcc} = compare_two_string(ASubList, BSubList, 0, []),
            case TempLength > Acc of
                true ->
                    {TempLength, TempStringAcc};
                false ->
                    {Acc, StringAcc}
            end
    end.


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
is_in_string(_Char, [], _Index) ->
    false;
is_in_string(Char, [Char|_Tail], Index) ->
    Index;
is_in_string(Char, [_OtherChar|Tail], Index) ->
    is_in_string(Char, Tail, Index + 1).


compare_two_string([], _BSubList, Length, StringAcc) ->
    {Length, lists:reverse(StringAcc)};
compare_two_string(_ASublist, [], Length, StringAcc) ->
    {Length, lists:reverse(StringAcc)};
compare_two_string([Char|ASublistTail], [Char|BSubListTail], Length, StringAcc) ->
    compare_two_string(ASublistTail, BSubListTail, Length + 1, [Char|StringAcc]);
compare_two_string([_CharA|_ASublistTail], [_CharB|_BSubListTail], Length, StringAcc) ->
    {Length, lists:reverse(StringAcc)}.





%%%===================================================================
%%% Internal functions
%%%===================================================================
