# riak_ensemble blocking_send_allow
## code definition
```
%% copy from the riak-ensemble_peer.erl
-spec blocking_send_all(any(), state()) -> {riak_ensemble_msg:future(), state()}.
blocking_send_all(Msg, State=#state{members=Members}) ->
    Peers = get_peers(Members, State),
    blocking_send_all(Msg, Peers, State).

-spec blocking_send_all(any(), peer_pids(), state()) -> {riak_ensemble_msg:future(), state()}.
blocking_send_all(Msg, Peers, State) ->
    blocking_send_all(Msg, Peers, quorum, State).

-spec blocking_send_all(any(), peer_pids(), _, state()) -> Result when
      Result :: {riak_ensemble_msg:future(), state()}.
blocking_send_all(Msg, Peers, Required, State) ->
    blocking_send_all(Msg, Peers, Required, undefined, State).

-spec blocking_send_all(any(), peer_pids(), _, Extra, state()) -> Result when
      Extra  :: riak_ensemble_msg:extra_check(),
      Result :: {riak_ensemble_msg:future(), state()}.
blocking_send_all(Msg, Peers, Required, Extra, State=#state{id=Id}) ->
    Views = views(State),
    {Future, Awaiting} = riak_ensemble_msg:blocking_send_all(Msg, Id, Peers, Views, Required, Extra),
    State2 = State#state{awaiting=Awaiting},
    {Future, State2}.

%% copy from the riak_ensemble_msg.erl
-spec blocking_send_all(msg(), peer_id(), peer_pids(), views())
                       -> {future(), msg_state()}.
blocking_send_all(Msg, Id, Peers, Views) ->
    blocking_send_all(Msg, Id, Peers, Views, quorum, undefined).

-spec blocking_send_all(msg(), peer_id(), peer_pids(), views(), required())
                       -> {future(), msg_state()}.
blocking_send_all(Msg, Id, Peers, Views, Required) when Required =/= undefined ->
    blocking_send_all(Msg, Id, Peers, Views, Required, undefined).

-spec blocking_send_all(msg(), peer_id(), peer_pids(), views(),
                        required(), extra_check()) -> {future(), msg_state()}.
blocking_send_all(Msg, Id, Peers, Views, Required, Extra) when Required =/= undefined ->
    ?OUT("~p: blocking_send_all to ~p: ~p~n", [Id, Peers, Msg]),
    MsgState = #msgstate{awaiting=undefined, timer=undefined, replies=[],
                         views=Views, id=Id, required=Required},
    Future = case Peers of
                 [{Id,_}] ->
                     undefined;
                 _ ->
                     spawn_link(fun() ->
                                        collector(Msg, Peers, Extra, MsgState)
                                end)
             end,
    {Future, MsgState}.

-spec collector(msg(), peer_pids(), extra_check(), msg_state()) -> ok.
collector(Msg, Peers, Extra, #msgstate{id=Id, views=Views, required=Required}) ->
    {ReqId, Request} = make_request(Msg),
    _ = [maybe_send_request(Id, Peer, ReqId, Request) || Peer={PeerId,_} <- Peers,
                                                         PeerId =/= Id],
    collect_replies(#collect{replies=[],
                             parent=undefined,
                             id=Id,
                             views=Views,
                             required=Required,
                             extra=Extra,
                             reqid=ReqId}).

-spec collect_replies(collect()) -> ok.
collect_replies(Collect=#collect{replies=Replies, reqid=ReqId}) ->
    receive
        {'$gen_all_state_event', Event} ->
            {reply, ReqId, Peer, Reply} = Event,
            Replies2 = [{Peer, Reply}|Replies],
            check_enough(Collect#collect{replies=Replies2});
        {waiting, From, Ref} when is_pid(From), is_reference(Ref) ->
            Parent = {From, Ref},
            check_enough(Collect#collect{parent=Parent})
    after ?ENSEMBLE_TICK ->
            maybe_timeout(Collect)
    end.

maybe_timeout(#collect{parent=undefined, replies=Replies, id=Id,
                       views=Views, required=Required, extra=Extra}) ->
    receive {waiting, From, Ref} ->
            case quorum_met(Replies, Id, Views, Required, Extra) of
                true ->
                    From ! {Ref, ok, Replies},
                    ok;
                _ ->
                    collect_timeout(Replies, {From, Ref})
            end
    end;
maybe_timeout(#collect{replies=Replies, parent=Parent}) ->
    collect_timeout(Replies, Parent).

-spec collect_timeout([peer_reply()], from()) -> ok.
collect_timeout(Replies, {From, Ref}) ->
    From ! {Ref, timeout, Replies},
    ok.

-spec check_enough(collect()) -> ok.
check_enough(Collect=#collect{parent=undefined}) ->
    collect_replies(Collect);
check_enough(Collect=#collect{id=Id,
                              replies=Replies,
                              parent={From,Ref}=Parent,
                              views=Views,
                              required=Required,
                              extra=Extra}) ->
    case quorum_met(Replies, Id, Views, Required, Extra) of
        true when Required =:= all_or_quorum ->
            %% If we've hit a quorum with all_or_quorum required, then
            %% we need to wait some additional length of time and see
            %% if we get replies from all.
            try_collect_all(Collect);
        true ->
            From ! {Ref, ok, Replies},
            ok;
        nack ->
            collect_timeout(Replies, Parent);
        false ->
            collect_replies(Collect)
    end.

-spec try_collect_all(#collect{}) -> _.
try_collect_all(Collect=#collect{reqid=ReqId}) ->
    Timeout = riak_ensemble_config:notfound_read_delay(),
    erlang:send_after(Timeout, self(), {try_collect_all_timeout, ReqId}),
    try_collect_all_impl(Collect).

try_collect_all_impl(Collect=#collect{id=Id,
                                      replies=Replies0,
                                      parent={From, Ref},
                                      reqid=ReqId,
                                      views=Views}) ->
    receive
        {'$gen_all_state_event', Event} ->
            {reply, ReqId, Peer, Reply} = Event,
            Replies = [{Peer, Reply}|Replies0],
            case quorum_met(Replies, Id, Views, all) of
                true ->
                    %% At this point we should be guaranteed to have already
                    %% gotten a parent that we can reply to:
                    ?OUT("Met quorum with Event ~p Replies ~p", [Event, Replies, Views]),
                    From ! {Ref, ok, Replies};
                false ->
                    ?OUT("Got additional message ~p but quorum still not met", [Event]),
                    try_collect_all(Collect#collect{replies=Replies});
                nack ->
                    %% Since we're waiting for all, we may see a nack from even
                    %% just a single negative response. But, we already know we
                    %% have a quorum of positive replies, so we can still send
                    %% back an 'ok' response with the replies we've gotten.
                    ?OUT("Got a nack! Returning replies so far: ~p", [Replies]),
                    From ! {Ref, ok, Replies}
            end;
        {try_collect_all_timeout, ReqId} ->
            ?OUT("Timed out waiting for try_collect_all", []),
            From ! {Ref, ok, Replies0}
    end.

-spec wait_for_quorum(future()) -> {quorum_met, [peer_reply()]} |
                                   {timeout, [peer_reply()]}.
wait_for_quorum(undefined) ->
    {quorum_met, []};
wait_for_quorum(Pid) ->
    Ref = make_ref(),
    Pid ! {waiting, self(), Ref},
    receive
        {Ref, ok, Replies} ->
            {Valid, _Nacks} = find_valid(Replies),
            {quorum_met, Valid};
        {Ref, timeout, Replies} ->
            {timeout, Replies}
    end.
```
## used example
```
%% copy from the riak-ensemble_peer.erl
leading(ping_quorum, From, State=#state{fact=Fact, id=Id, members=Members,
                                        tree_ready=TreeReady}) ->
    NewFact = increment_sequence(Fact),
    State2 = local_commit(NewFact, State),
    {Future, State3} = blocking_send_all({commit, NewFact}, State2),
    Extra = case lists:member(Id, Members) of
                true  -> [{Id,ok}];
                false -> []
            end,
    spawn_link(fun() ->
                       %% TODO: Should this be hardcoded?
                       timer:sleep(1000),
                       Result = case wait_for_quorum(Future) of
                                    {quorum_met, Replies} ->
                                        %% io:format("met: ~p~n", [Replies]),
                                        Extra ++ Replies;
                                    {timeout, _Replies} ->
                                        %% io:format("timeout~n"),
                                        Extra
                                end,
                       gen_fsm:reply(From, {Id, TreeReady, Result})
               end),
    {next_state, leading, State3};

wait_for_quorum(Future) ->
    riak_ensemble_msg:wait_for_quorum(Future).

%% copy from the riak_ensemble_msg.erl
-spec wait_for_quorum(future()) -> {quorum_met, [peer_reply()]} |
                                   {timeout, [peer_reply()]}.
wait_for_quorum(undefined) ->
    {quorum_met, []};
wait_for_quorum(Pid) ->
    Ref = make_ref(),
    Pid ! {waiting, self(), Ref},
    receive
        {Ref, ok, Replies} ->
            {Valid, _Nacks} = find_valid(Replies),
            {quorum_met, Valid};
        {Ref, timeout, Replies} ->
            {timeout, Replies}
    end.
```
