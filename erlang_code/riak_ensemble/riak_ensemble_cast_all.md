# riak_ensemble cast_all function

```
%% copy from riak_ensemble_peer.erl
-spec cast_all(_, state()) -> ok.
cast_all(Msg, State=#state{id=Id, members=Members}) ->
    Peers = get_peers(Members, State),
    riak_ensemble_msg:cast_all(Msg, Id, Peers).

%% copy from the riak_ensemble_msg.erl
-spec cast_all(msg(), peer_id(), peer_pids()) -> ok.
cast_all(Msg, Id, Peers) ->
    ?OUT("~p/~p: casting to ~p: ~p~n", [Id, self(), Peers, Msg]),
    _ = [maybe_send_cast(Id, Peer, Msg) || Peer={PeerId,_} <- Peers,
                                           PeerId =/= Id],
    ok.

-spec maybe_send_cast(peer_id(), {peer_id(), maybe_pid()}, msg()) -> ok.
-ifdef(TEST).

maybe_send_cast(Id, {PeerId, PeerPid}, Event) ->
    case riak_ensemble_test:maybe_drop(Id, PeerId) of
        true ->
            %% TODO: Consider nacking instead
            io:format("Dropping ~p -> ~p~n", [Id, PeerId]),
            ok;
        false ->
            send_cast({PeerId, PeerPid}, Event)
    end.

-else.

maybe_send_cast(_Id, {PeerId, PeerPid}, Event) ->
    send_cast({PeerId, PeerPid}, Event).

-endif.

%%%===================================================================

-spec send_cast({peer_id(), maybe_pid()}, msg()) -> ok.
send_cast({_PeerId, PeerPid}, Event) ->
    case PeerPid of
        undefined ->
            ok;
        _ ->
            ?OUT("~p: Sending to ~p: ~p~n", [self(), _PeerId, Event]),
            gen_fsm:send_event(PeerPid, Event)
    end.
```

## used example
```
%% copy from riak_ensemble_peer.erl
maybe_repair(Key, Latest, Replies, State=#state{id=Id}) ->
    %% TODO: Should only send puts to peers that are actually divergent.
    ShouldRepair = lists:any(fun({_, nack}) ->
                                     false;
                                ({_Peer, Obj}) when (Obj =:= Latest) ->
                                     false;
                                ({_Peer, _Obj}) ->
                                     true
                             end, Replies),
    case ShouldRepair of
        true ->
            %% TODO: Following is kinda ugly, but works without code change.
            Epoch = epoch(State),
            Dummy = spawn(fun() -> ok end),
            DummyFrom = {Dummy, undefined},
            ok = cast_all({put, Key, Latest, Id, Epoch, DummyFrom}, State);
        false ->
            ok
    end.

nack({put, _, _, _, _, From}, State) ->
    ?OUT("~p: sending nack to ~p~n", [State#state.id, From]),
    reply(From, nack, State);

following_kv({put, Key, Obj, Peer, Epoch, From}, State) ->
    case valid_request(Peer, Epoch, State) of
        true ->
            State2 = do_local_put(From, Key, Obj, State),
            {next_state, following, State2};
        false ->
            ?OUT("~p: sending nack to ~p for invalid request: ~p != ~p~n", [State#state.id, Peer,
                                                                            {Peer, Epoch},
                                                                            {leader(State),
                                                                             epoch(State)}]),
            reply(From, nack, State),
            {next_state, following, State}
    end;
```
