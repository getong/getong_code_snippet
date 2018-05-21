# riak_ensemble peer send vote msg

## send_all/2 function
```
%% copy from the riak_ensemble_peer.erl
-spec send_all(_,state()) -> state().
send_all(Msg, State) ->
    send_all(Msg, quorum, State).

send_all(Msg, Required, State=#state{members=Members}) ->
    send_peers(Msg, Members, Required, State).

send_peers(Msg, Members, Required, State) ->
    Views = views(State),
    send_peers(Msg, Members, Required, Views, State).

send_peers(Msg, Members, Required, Views, State=#state{id=Id}) ->
    Peers = get_peers(Members, State),
    Awaiting = riak_ensemble_msg:send_all(Msg, Id, Peers, Views, Required),
    State#state{awaiting=Awaiting}.

%% copy from the riak_ensemble_msg.erl
-spec send_all(msg(), peer_id(), peer_pids(), views()) -> msg_state().
send_all(Msg, Id, Peers, Views) ->
    send_all(Msg, Id, Peers, Views, quorum).

-spec send_all(msg(), peer_id(), peer_pids(), views(), required()) -> msg_state().
send_all(_Msg, Id, _Peers=[{Id,_}], _Views, _Required) ->
    ?OUT("~p: self-sending~n", [Id]),
    gen_fsm:send_event(self(), {quorum_met, []}),
    #msgstate{awaiting=undefined, timer=undefined, replies=[], id=Id};
send_all(Msg, Id, Peers, Views, Required) ->
    ?OUT("~p/~p: sending to ~p: ~p~n", [Id, self(), Peers, Msg]),
    {ReqId, Request} = make_request(Msg),
    _ = [maybe_send_request(Id, Peer, ReqId, Request) || Peer={PeerId,_} <- Peers,
                                                         PeerId =/= Id],
    Timer = send_after(?ENSEMBLE_TICK, self(), quorum_timeout),
    #msgstate{awaiting=ReqId, timer=Timer, replies=[], id=Id, views=Views,
              required=Required}.

-spec make_request(msg()) -> {reqid(), tuple()}.
make_request(Msg) ->
    ReqId = make_ref(),
    From = make_from(self(), ReqId),
    Request = if is_tuple(Msg) ->
                      erlang:append_element(Msg, From);
                 true ->
                      {Msg, From}
              end,
    {ReqId, Request}.

-spec make_from(pid(), reqid()) -> msg_from().
make_from(Pid, ReqId) ->
    {riak_ensemble_msg, Pid, ReqId}.

-spec cast_all(msg(), peer_id(), peer_pids()) -> ok.
cast_all(Msg, Id, Peers) ->
    ?OUT("~p/~p: casting to ~p: ~p~n", [Id, self(), Peers, Msg]),
    _ = [maybe_send_cast(Id, Peer, Msg) || Peer={PeerId,_} <- Peers,
                                           PeerId =/= Id],
    ok.

%%%===================================================================

-spec maybe_send_request(peer_id(), {peer_id(), maybe_pid()}, reqid(), msg()) -> ok.
-ifdef(TEST).

maybe_send_request(Id, {PeerId, PeerPid}, ReqId, Event) ->
    case riak_ensemble_test:maybe_drop(Id, PeerId) of
        true ->
            %% TODO: Consider nacking instead
            io:format("Dropping ~p -> ~p~n", [Id, PeerId]),
            ok;
        false ->
            send_request({PeerId, PeerPid}, ReqId, Event)
    end.

-else.

maybe_send_request(_Id, {PeerId, PeerPid}, ReqId, Event) ->
    send_request({PeerId, PeerPid}, ReqId, Event).

-endif.

%%%===================================================================

-spec send_request({peer_id(), maybe_pid()}, reqid(), msg()) -> ok.
send_request({PeerId, PeerPid}, ReqId, Event) ->
    case PeerPid of
        undefined ->
            ?OUT("~p: Sending offline nack for ~p~n", [self(), PeerId]),
            From = make_from(self(), ReqId),
            reply(From, PeerId, nack);
        _ ->
            ?OUT("~p: Sending to ~p: ~p~n", [self(), PeerId, Event]),
            gen_fsm:send_event(PeerPid, Event)
    end.
```
The timeout msg:
```
%% copy from the riak_ensemble_peer.erl
handle_info(quorum_timeout, StateName, State) ->
    State2 = quorum_timeout(State),
    {next_state, StateName, State2};

quorum_timeout(State=#state{awaiting=undefined}) ->
    State;
quorum_timeout(State=#state{awaiting=Awaiting}) ->
    Awaiting2 = riak_ensemble_msg:quorum_timeout(Awaiting),
    State#state{awaiting=Awaiting2}.

%% copy from the riak_ensemble_msg.erl
-spec quorum_timeout(msg_state()) -> msg_state().
quorum_timeout(#msgstate{replies=Replies}) ->
    {Valid, _Nacks} = find_valid(Replies),
    gen_fsm:send_event(self(), {timeout, Valid}),
    #msgstate{awaiting=undefined, timer=undefined, replies=[]}.


%% copy from the riak_ensemble_peer.erl
probe({timeout, Replies}, State=#state{fact=Fact}) ->
    Latest = latest_fact(Replies, Fact),
    State2 = State#state{fact=Latest},
    State3 = check_views(State2),
    probe(delay, State3);
probe(delay, State) ->
    State2 = set_timer(?PROBE_DELAY, probe_continue, State),
    {next_state, probe, State2};
probe(probe_continue, State) ->
    probe(init, State);
```

used example:
```
%% copy from the riak_ensemble_peer.erl
probe(init, State) ->
    lager:debug("~p: probe init", [State#state.id]),
    State2 = set_leader(undefined, State),
    case is_pending(State2) of
        true ->
            pending(init, State2);
        false ->
            State3 = send_all(probe, State2),
            {next_state, probe, State3}
    end;

common({probe, From}, State=#state{fact=Fact}, StateName) ->
    reply(From, Fact, State),
    {next_state, StateName, State};

-spec reply(riak_ensemble_msg:msg_from(), any(), state()) -> ok.
reply(From, Reply, #state{id=Id}) ->
    riak_ensemble_msg:reply(From, Id, Reply).

%% copy from the riak_ensemble_msg.erl
-spec reply(msg_from(), peer_id(), any()) -> ok.
reply({riak_ensemble_msg, Sender, ReqId}, Id, Reply) ->
    gen_fsm:send_all_state_event(Sender, {reply, ReqId, Id, Reply}).

%% copy from the riak_ensemble_peer.erl
handle_event({reply, ReqId, Peer, Reply}, StateName, State) ->
    State2 = handle_reply(ReqId, Peer, Reply, State),
    {next_state, StateName, State2};

%% copy from the riak_ensemble_msg.erl
-spec handle_reply(any(), peer_id(), any(), msg_state()) -> msg_state().
handle_reply(ReqId, Peer, Reply, MsgState=#msgstate{awaiting=Awaiting}) ->
    case ReqId == Awaiting of
        true ->
            add_reply(Peer, Reply, MsgState);
        false ->
            MsgState
    end.

-spec add_reply(peer_id(), any(), msg_state()) -> msg_state().
add_reply(Peer, Reply, MsgState=#msgstate{timer=Timer}) ->
    Replies = [{Peer, Reply} | MsgState#msgstate.replies],
    case quorum_met(Replies, MsgState) of
        true ->
            cancel_timer(Timer),
            {Valid, _Nacks} = find_valid(Replies),
            gen_fsm:send_event(self(), {quorum_met, Valid}),
            MsgState#msgstate{replies=[], awaiting=undefined, timer=undefined};
        false ->
            MsgState#msgstate{replies=Replies};
        nack ->
            cancel_timer(Timer),
            quorum_timeout(MsgState#msgstate{replies=Replies})
    end.

-spec quorum_timeout(msg_state()) -> msg_state().
quorum_timeout(#msgstate{replies=Replies}) ->
    {Valid, _Nacks} = find_valid(Replies),
    gen_fsm:send_event(self(), {timeout, Valid}),
    #msgstate{awaiting=undefined, timer=undefined, replies=[]}.

```


## example

``` erlang
X = riak_ensemble_msg:blocking_send_all(exchange, Id, Peers,
                                       Views, Required),
```
copy from riak_ensemble_exchange.erl

``` erlang
common({exchange, From}, State, StateName) ->
    case State#state.tree_trust of
        true ->
            reply(From, ok, State);
        false ->
            reply(From, nack, State)
    end,
    {next_state, StateName, State};
```
copy from riak_ensemble_peer.erl

The `Msg` variable now is `exchange`, and `exchange` is not tuple, so the new msg made by `make_request(Msg)` is `{exchange, From}`.
And in the riak_ensemble_peer.erl module file, the common/3 function handle the `{exchange, From}` msg.
That is it.
