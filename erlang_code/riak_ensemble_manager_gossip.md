#gossip

## ensemble operation
```
gossip(CS) ->
    gen_server:call(?MODULE, {gossip, CS}, infinity).

gossip_pending(Ensemble, Vsn, Views) ->
    gen_server:cast(?MODULE, {gossip_pending, Ensemble, Vsn, Views}).

-spec update_ensemble(ensemble_id(), peer_id(), views(), vsn()) -> ok.
update_ensemble(Ensemble, Leader, Views, Vsn) ->
    gen_server:call(?MODULE, {update_ensemble, Ensemble, Leader, Views, Vsn}, infinity).

handle_call({gossip, OtherCS}, _From, State) ->
    NewCS = merge_gossip(OtherCS, State),
    save_state_reply(NewCS, State);

handle_call({update_ensemble, Ensemble, Leader, Views, Vsn}, _From, State=#state{cluster_state=CS}) ->
    case riak_ensemble_state:update_ensemble(Vsn, Ensemble, Leader, Views, CS) of
        error ->
            {reply, error, State};
        {ok, NewCS} ->
            save_state_reply(NewCS, State)
    end;

handle_cast({gossip_pending, Ensemble, Vsn, Views}, State=#state{cluster_state=CS}) ->
    case riak_ensemble_state:set_pending(Vsn, Ensemble, Views, CS) of
        error ->
            {noreply, State};
        {ok, NewCS} ->
            save_state_noreply(NewCS, State)
    end;

-spec merge_gossip(cluster_state(), state()) -> cluster_state().
merge_gossip(OtherCS, #state{cluster_state=CS}) ->
    case CS of
        undefined ->
            OtherCS;
        _ ->
            riak_ensemble_state:merge(CS, OtherCS)
    end.

save_state_noreply(NewCS, State) ->
    State2 = State#state{cluster_state=NewCS},
    case maybe_save_state(State2) of
        ok ->
            State3 = state_changed(State2),
            {noreply, State3};
        {error,_} ->
            %% Failed to save, keep original state
            {noreply, State}
    end.

save_state_reply(NewCS, State) ->
    State2 = State#state{cluster_state=NewCS},
    case maybe_save_state(State2) of
        ok ->
            State3 = state_changed(State2),
            {reply, ok, State3};
        {error,_} ->
            %% Failed to save, keep original state
            {reply, ok, State}
    end.

maybe_save_state(State=#state{cluster_state=NewCS}) ->
    OldState = reload_state(),
    OldCS = OldState#state.cluster_state,
    if OldCS =:= NewCS ->
            ok;
       true ->
            save_state(State)
    end.

-spec reload_state() -> state().
reload_state() ->
    case load_saved_state() of
        {ok, State} ->
            %% io:format("reloaded~n"),
            State;
        not_found ->
            initial_state()
    end.

-spec initial_state() -> state().
initial_state() ->
    ets:insert(?ETS, {enabled, false}),
    ClusterName = {node(), erlang:now()},
    CS = riak_ensemble_state:new(ClusterName),
    State=#state{version=0,
                 ensemble_data=[],
                 remote_peers=[],
                 cluster_state=CS},
    State.

-spec load_saved_state() -> not_found | {ok, state()}.
load_saved_state() ->
    try
        {ok, CS} = riak_ensemble_storage:get(manager),
        true = riak_ensemble_state:is_state(CS),
        State = #state{ensemble_data=[],
                       remote_peers=[],
                       cluster_state=CS},
        {ok, State}
    catch
        _:_ ->
            not_found
    end.

```
## tick to gossip operation

```
-spec init([]) -> {ok, state()}.
init([]) ->
    _ = ets:new(?ETS, [named_table, public, {read_concurrency, true}, {write_concurrency, true}]),
    State = reload_state(),
    schedule_tick(),
    true = ets:insert(?ETS, {cluster_state, State#state.cluster_state}),
    gen_server:cast(self(), init),
    {ok, State}.

-spec schedule_tick() -> 'ok'.
schedule_tick() ->
    Time = 2000,
    _ = erlang:send_after(Time, self(), tick),
    ok.

handle_info(tick, State) ->
    State2 = tick(State),
    schedule_tick(),
    {noreply, State2};

-spec tick(state()) -> state().
tick(State) ->
    request_remote_peers(State),
    send_gossip(State),
    State.

-spec send_gossip(state()) -> ok.
send_gossip(#state{cluster_state=CS}) ->
    Members = riak_ensemble_state:members(CS) -- [node()],
    Shuffle = riak_ensemble_util:shuffle(Members),
    Nodes = lists:sublist(Shuffle, 10),
    _ = [gen_server:cast({?MODULE, Node}, {gossip, CS}) || Node <- Nodes],
    ok.

handle_cast({gossip, OtherCS}, State) ->
    NewCS = merge_gossip(OtherCS, State),
    save_state_noreply(NewCS, State);

-spec request_remote_peers(state()) -> ok.
request_remote_peers(State=#state{remote_peers=Remote}) ->
    Root = case rleader() of
               undefined ->
                   [];
               Leader ->
                   [{root, Leader}]
           end,
    WantedRemote = Root ++ wanted_remote_peers(State),
    Need = ordsets:subtract(ordsets:from_list(WantedRemote),
                            orddict:fetch_keys(Remote)),
    _ = [request_peer_pid(Ensemble, Peer) || {Ensemble, Peer} <- Need],
    ok.

-spec wanted_remote_peers(state()) -> [{ensemble_id(), peer_id()}].
wanted_remote_peers(#state{cluster_state=CS}) ->
    Ensembles = riak_ensemble_state:ensembles(CS),
    Pending = riak_ensemble_state:pending(CS),
    ThisNode = node(),
    [{Ensemble, Peer} || {Ensemble, #ensemble_info{views=Views}} <- Ensembles,
                         AllPeers <- [compute_all_members(Ensemble, Pending, Views)],
                         lists:keymember(ThisNode, 2, AllPeers),
                         Peer={_, Node} <- AllPeers,
                         Node =/= ThisNode].

-spec request_peer_pid(ensemble_id(), peer_id()) -> ok.
request_peer_pid(Ensemble, PeerId={_, Node}) ->
    %% io:format("Requesting ~p/~p~n", [Ensemble, PeerId]),
    %% riak_ensemble_util:cast_unreliable({?MODULE, Node},
    %%                                    {request_peer_pid, self(), {Ensemble, PeerId}}).
    typed_cast(Node, {request_peer_pid, self(), {Ensemble, PeerId}}).

-spec typed_cast(node() | pid(), cast_msg()) -> ok.
typed_cast(Pid, Msg) when is_pid(Pid) ->
    gen_server:cast(Pid, Msg);
typed_cast(Node, Msg) when is_atom(Node) ->
    gen_server:cast({?MODULE, Node}, Msg).

handle_cast({request_peer_pid, From, PeerId}, State) ->
    %% TODO: Confusing that we use {Ensemble, PeerId} as PeerId
    {Ensemble, Id} = PeerId,
    case get_peer_pid(Ensemble, Id) of
        undefined ->
            ok;
        Pid ->
            typed_cast(From, {peer_pid, PeerId, Pid})
    end,
    {noreply, State};

handle_cast({peer_pid, Peer, Pid}, State=#state{remote_peers=Remote}) ->
    Remote2 = orddict:store(Peer, Pid, Remote),
    ets:insert(?ETS, {{remote_pid, Peer}, Pid}),
    erlang:monitor(process, Pid),
    %% io:format("Tracking remote peer: ~p :: ~p~n", [Peer, Pid]),
    {noreply, State#state{remote_peers=Remote2}};

```
