#riak_ensemble kv put get

## fact
```
%% in the riak_ensemble_storage.erl
-spec get(term()) -> {ok, term()} | not_found.
get(Key) ->
    try
        Value = ets:lookup_element(?ETS, Key, 2),
        {ok, Value}
    catch
        _:_ ->
            %% Retry through the server in case data is being loaded
            gen_server:call(?MODULE, {get, Key}, infinity)
    end.

-spec put(term(), term()) -> true.
put(Key, Value) ->
    ets:insert(?ETS, {Key, Value}).

%% in the riak_ensemble_peer.erl
-spec load_saved_fact(_,_) -> not_found | {ok,_}.
load_saved_fact(Ensemble, Id) ->
    riak_ensemble_storage:get({Ensemble, Id}).

-spec save_fact(state()) -> ok | {error,_}.
save_fact(#state{ensemble=Ensemble, id=Id, fact=Fact}) ->
    try
        true = riak_ensemble_storage:put({Ensemble, Id}, Fact),
        ok = riak_ensemble_storage:sync()
    catch
        _:Err ->
            %% _ = lager:error("Failed saving ensemble ~p state to ~p: ~p",
            %%                 [{Ensemble, Id}, File, Err]),
            {error, Err}
    end.
```
## object key
```
%% in the riak_ensemble_peer.erl
do_put_fsm(Key, Fun, Args, From, Self, State=#state{tree=Tree}) ->
    case riak_ensemble_peer_tree:get(Key, Tree) of
        corrupted ->
            %% io:format("Tree corrupted (put)!~n"),
            send_reply(From, failed),
            gen_fsm:sync_send_event(Self, tree_corrupted, infinity);
        KnownHash ->
            do_put_fsm(Key, Fun, Args, From, Self, KnownHash, State)
    end.

update_hash(Key, ObjHash, State=#state{tree=Tree}) ->
    case riak_ensemble_peer_tree:insert(Key, ObjHash, Tree) of
        corrupted ->
            %% io:format("Tree corrupted (update_hash)!~n"),
            {corrupted, State};
        ok ->
            {ok, State}
    end.

%% in the riak_ensemble_peer_tree
-spec do_get(_,state()) -> {any(), state()}.
do_get(Key, State=#state{tree=Tree}) ->
    case synctree:get(Key, Tree) of
        {corrupted, Level, Bucket} ->
            State2 = State#state{corrupted={Level, Bucket}},
            {corrupted, State2};
        Other ->
            {Other, State}
    end.

-spec do_insert(_,_,state()) -> {ok, state()} | {corrupted, state()}.
do_insert(Key, ObjHash, State=#state{tree=Tree}) ->
    case synctree:insert(Key, ObjHash, Tree) of
        {corrupted, Level, Bucket} ->
            State2 = State#state{corrupted={Level, Bucket}},
            {corrupted, State2};
        NewTree ->
            %% io:format("Hash updated: ~p :: ~p~n", [NewTree, synctree:top_hash(NewTree)]),
            State2 = State#state{tree=NewTree},
            {ok, State2}
    end.

%% in the synctree.erl
-spec get(key(), tree()) -> value() | notfound | corrupted().
get(Key, Tree) ->
    TopHash = top_hash(Tree),
    case TopHash of
        undefined ->
            notfound;
        _ ->
            Segment = get_segment(Key, Tree),
            case get_path(Segment, Tree) of
                {corrupted,_,_}=Error ->
                    Error;
                [{_, Hashes}|_] ->
                    orddict_find(Key, notfound, Hashes)
            end
    end.

-spec insert(key(), value(), tree()) -> tree() | corrupted().
insert(Key, Value, Tree) when is_binary(Value) ->
    Segment = get_segment(Key, Tree),
    case get_path(Segment, Tree) of
        {corrupted,_,_}=Error ->
            Error;
        Path ->
            {TopHash, Updates} = update_path(Path, Key, Value, []),
            Tree2 = m_store(Updates, Tree),
            Tree2#tree{top_hash=TopHash}
    end.

get_segment(Key, #tree{segments=Segments}) ->
    <<HashKey:128/integer>> = crypto:hash(md5, ensure_binary(Key)),
    HashKey rem Segments.

get_path(Segment, Tree=#tree{shift=Shift, shift_max=N}) ->
    TopHash = top_hash(Tree),
    get_path(N, 1, Shift, Segment, [{0, TopHash}], Tree, []).

get_path(N, Level, Shift, Segment, UpHashes, Tree, Acc) ->
    Bucket = Segment bsr N,
    Expected = orddict_find(Bucket, undefined, UpHashes),
    {ok, Hashes} = m_fetch({Level, Bucket}, [], Tree),
    Acc2 = [{{Level, Bucket}, Hashes}|Acc],
    Verify = verify_hash(Expected, Hashes),
    case {Verify, N} of
        {false, _} ->
            lager:warning("Corrupted at ~p/~p~n", [Level, Bucket]),
            {corrupted, Level, Bucket};
        {_, 0} ->
            Acc2;
        _ ->
            get_path(N-Shift, Level+1, Shift, Segment, Hashes, Tree, Acc2)
    end.

verify_hash(undefined, []) ->
    true;
verify_hash(undefined, _Actual) ->
    %% io:format("Expected (undef): []~n"),
    %% io:format("Actual:   ~p~n", [_Actual]),
    false;
verify_hash(Expected, Hashes) ->
    %% Note: when we add support for multiple hash functions, update this
    %%       function to compute Actual using the same function that was
    %%       previously used to compute Expected.
    Actual = hash(Hashes),
    case Expected of
        Actual ->
            true;
        _ ->
            %% io:format("Expected: ~p~n", [Expected]),
            %% io:format("Actual:   ~p~n", [Actual]),
            false
    end.

orddict_find(Key, Default, L) ->
    case lists:keyfind(Key, 1, L) of
        false ->
            Default;
        {_, Value} ->
            Value
    end.

%% in the synctree_ets.erl
-spec fetch(_, _, state()) -> {ok, _}.
fetch(Key, Default, ?STATE{ets=T}) ->
    case ets:lookup(T, Key) of
        [] ->
            {ok, Default};
        [{_, Value}] ->
            {ok, Value}
    end.

-spec store(_, _, state()) -> state().
store(Key, Val, State=?STATE{ets=T}) ->
    _ = ets:insert(T, {Key, Val}),
    State.

-spec store([{_,_}], state()) -> state().
store(Updates, State=?STATE{ets=T}) ->
    %% _ = ets:insert(T, Updates),
    Inserts = [case Update of
                   {put, Key, Val} ->
                       {Key, Val};
                   {delete, Key} ->
                       {Key, deleted}
               end || Update <- Updates],
    _ = ets:insert(T, Inserts),
    _ = [ets:delete_object(T, {Key, deleted}) || {delete, Key} <- Updates],
    State.

```


## object
```
%% in the riak_ensemble_peer.erl
mod_get(Key, From, State=#state{mod=Mod, modstate=ModState, id=Id}) ->
    ModState2 = Mod:get(Key, {From, Id}, ModState),
    State#state{modstate=ModState2}.

mod_put(Key, Obj, From, State=#state{mod=Mod, modstate=ModState, id=Id}) ->
    ModState2 = Mod:put(Key, Obj, {From, Id}, ModState),
    State#state{modstate=ModState2}.

get_obj(X, Obj, Mod) when is_atom(Mod) ->
    riak_ensemble_backend:get_obj(Mod, X, Obj);
get_obj(X, Obj, #state{mod=Mod, modstate=_ModState}) ->
    riak_ensemble_backend:get_obj(Mod, X, Obj).

set_obj(X, Val, Obj, #state{mod=Mod, modstate=_ModState}) ->
    riak_ensemble_backend:set_obj(Mod, X, Val, Obj).

%% in the riak_ensemble_basic_backend.erl
-spec get(key(), riak_ensemble_backend:from(), state()) -> state().
get(Key, From, State=#state{data=Data}) ->
    Reply = case orddict:find(Key, Data) of
                {ok, Value} ->
                    Value;
                error ->
                    notfound
            end,
    riak_ensemble_backend:reply(From, Reply),
    State.

-spec put(key(), obj(), riak_ensemble_backend:from(), state()) -> state().
put(Key, Obj, From, State=#state{savefile=File, data=Data}) ->
    Data2 = orddict:store(Key, Obj, Data),
    save_data(File, Data2),
    riak_ensemble_backend:reply(From, Obj),
    State#state{data=Data2}.
```
