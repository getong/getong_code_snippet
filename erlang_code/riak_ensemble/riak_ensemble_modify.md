# riak_ensemble kmodify
```
%% copy from riak_ensemble_peer.erl
-spec kmodify(node(), target(), key(), modify_fun(), term(), timeout()) -> std_reply().
kmodify(Node, Target, Key, ModFun, Default, Timeout) ->
    F = fun ?MODULE:do_kmodify/4,
    Result = riak_ensemble_router:sync_send_event(Node, Target, {put, Key, F, [ModFun, Default]}, Timeout),
    ?OUT("kmodify(~p): ~p~n", [Key, Result]),
    Result.

do_kmodify(Obj, NextSeq, State, [ModFun, Default]) ->
    Value = get_value(Obj, Default, State),
    Vsn = {epoch(State), NextSeq},
    New = case ModFun of
              {Mod, Fun, Args} ->
                  Mod:Fun(Vsn, Value, Args);
              _ ->
                  ModFun(Vsn, Value)
          end,
    case New of
        failed ->
            failed;
        _ ->
            {ok, set_obj(value, New, Obj, State)}
    end.

leading_kv({put, _Key, _Fun, _Args}, From, State=#state{tree_ready=false}) ->
    fail_request(From, State);
leading_kv({put, Key, Fun, Args}, From, State) ->
    Self = self(),
    async(Key, State, fun() -> do_put_fsm(Key, Fun, Args, From, Self, State) end),
    {next_state, leading, State};

async(Key, State, Fun) ->
    Workers = State#state.workers,
    Pick = erlang:phash2(Key, tuple_size(Workers)),
    Worker = element(Pick+1, Workers),
    Worker ! {async, Fun},
    ok.

do_put_fsm(Key, Fun, Args, From, Self, State=#state{tree=Tree}) ->
    case riak_ensemble_peer_tree:get(Key, Tree) of
        corrupted ->
            %% io:format("Tree corrupted (put)!~n"),
            send_reply(From, failed),
            gen_fsm:sync_send_event(Self, tree_corrupted, infinity);
        KnownHash ->
            do_put_fsm(Key, Fun, Args, From, Self, KnownHash, State)
    end.

do_put_fsm(Key, Fun, Args, From, Self, KnownHash, State) ->
    %% TODO: Timeout should be configurable per request
    Local = local_get(Self, Key, ?LOCAL_GET_TIMEOUT),
    State2 = State#state{self=Self},
    case is_current(Local, Key, KnownHash, State2) of
        local_timeout ->
            %% TODO: Should this send a request_failed?
            %% gen_fsm:sync_send_event(Self, request_failed, infinity),
            send_reply(From, unavailable);
        true ->
            do_modify_fsm(Key, Local, Fun, Args, From, State2);
        false ->
            case update_key(Key, Local, KnownHash, State2) of
                {ok, Current, _State3} ->
                    do_modify_fsm(Key, Current, Fun, Args, From, State2);
                {corrupted, _State2} ->
                    send_reply(From, failed),
                    gen_fsm:sync_send_event(Self, tree_corrupted, infinity);
                {failed, _State3} ->
                    gen_fsm:sync_send_event(Self, request_failed, infinity),
                    send_reply(From, unavailable)
            end
    end.

%% -spec do_modify_fsm(_,_,fun((_,_) -> any()),{_,_},state()) -> ok.
do_modify_fsm(Key, Current, Fun, Args, From, State=#state{self=Self}) ->
    case modify_key(Key, Current, Fun, Args, State) of
        {ok, New, _State2} ->
            send_reply(From, {ok, New});
        {corrupted, _State2} ->
            send_reply(From, failed),
            gen_fsm:sync_send_event(Self, tree_corrupted, infinity);
        {precondition, _State2} ->
            send_reply(From, failed);
        {failed, _State2} ->
            gen_fsm:sync_send_event(Self, request_failed, infinity),
            send_reply(From, timeout)
    end.

modify_key(Key, Current, Fun, Args, State) ->
    Seq = obj_sequence(State),
    FunResult = case Args of
                    [] ->
                        Fun(Current, Seq, State);
                    _ ->
                        Fun(Current, Seq, State, Args)
                end,
    case FunResult of
        {ok, New} ->
            case put_obj(Key, New, Seq, State) of
                {ok, Result, State2} ->
                    {ok, Result, State2};
                {corrupted, State2} ->
                    {corrupted, State2};
                {failed, State2} ->
                    {failed, State2}
            end;
        failed ->
            {precondition, State}
    end.

-spec put_obj(_,obj(),seq(),state()) -> {ok, obj(), state()} | {failed,state()} | {corrupted,state()}.
put_obj(Key, Obj, Seq, State=#state{id=Id, members=Members, self=Self}) ->
    Epoch = epoch(State),
    Obj2 = increment_obj(Key, Obj, Seq, State),
    Peers = get_peers(Members, State),
    {Future, State2} = blocking_send_all({put, Key, Obj2, Id, Epoch}, Peers, State),
    case local_put(Self, Key, Obj2, ?LOCAL_PUT_TIMEOUT) of
        failed ->
            lager:warning("Failed local_put for Key ~p, Id = ~p", [Key, Id]),
            gen_fsm:sync_send_event(Self, request_failed, infinity),
            {failed, State2};
        Local ->
            case wait_for_quorum(Future) of
                {quorum_met, _Replies} ->
                    ObjHash = get_obj_hash(Key, Local, State2),
                    case update_hash(Key, ObjHash, State2) of
                        {ok, State3} ->
                            case send_update_hash(Key, ObjHash, State3) of
                                {ok, State4} ->
                                    {ok, Local, State4};
                                {failed, State4} ->
                                    {failed, State4}
                            end;
                        {corrupted, State3} ->
                            {corrupted, State3}
                    end;
                {timeout, _Replies} ->
                    {failed, State2}
            end
    end.

send_update_hash(Key, ObjHash, State) ->
    case riak_ensemble_config:synchronous_tree_updates() of
        false ->
            Msg = {update_hash, Key, ObjHash, undefined},
            cast_all(Msg, State),
            {ok, State};
        true ->
            Msg = {update_hash, Key, ObjHash},
            {Future, State2} = blocking_send_all(Msg, State),
            case wait_for_quorum(Future) of
                {quorum_met, _Replies} ->
                    {ok, State2};
                {timeout, _Replies} ->
                    {failed, State2}
            end
    end.
```
