# aten

Keep the heartbeat time to a array, and get the mean time by the array.
```
-spec sample_now(state()) -> state().
sample_now(State) ->
    append(ts(), State).

-spec get_failure_probability(state()) -> float().
get_failure_probability(State) ->
    failure_prob_at(ts(), State).

%% Internal

append(Ts, #state{freshness = undefined} = State) ->
    State#state{freshness = Ts};
append(Ts0, #state{freshness = F,
                   samples = Samples,
                   next_index = Next} = State) when is_number(F) ->
    Ts = Ts0 - F,
    State#state{samples = array:set(Next, Ts, Samples),
                next_index = (Next + 1) rem ?WINDOW_SIZE,
                freshness = Ts0}.

failure_prob_at(_At, #state{freshness = undefined}) ->
    0.0;
failure_prob_at(At, #state{freshness = F,
                           factor = A,
                           samples = Samples}) ->
    T = At - F,
    {TotNum, SmallNum} = array:foldl(fun(_, undefined, Acc) ->
                                             Acc;
                                        (_, S, {Tot, Smaller}) when S * A =< T ->
                                             {Tot+1, Smaller+1};
                                        (_, _S, {Tot, Smaller}) ->
                                             {Tot+1, Smaller}
                                     end, {0, 0}, Samples),
    SmallNum / max(1, TotNum). % avoid div/0

ts() ->
    % TODO: should we use erlang monotonic time instead?
    % It probably doesn't matter
    erlang:system_time(microsecond).
```
