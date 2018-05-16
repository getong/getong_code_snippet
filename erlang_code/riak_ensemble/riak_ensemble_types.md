# riak_ensemble types

## types are defined in riak_ensemble_types.hrl

``` erlang
-type ensemble_id() :: term().
-type peer_id() :: {term(), node()}.

-record(ensemble_info, {vsn                                   :: vsn(),
                        mod     = riak_ensemble_basic_backend :: module(),
                        args    = []                          :: [any()],
                        leader                                :: leader_id(),
                        views                                 :: [[peer_id()]],
                        seq                                   :: {integer(), integer()}
                       }).

```

## update member can be {del, id}

``` erlang
update_view(Changes, Members, View, Cluster) ->
    update_view(Changes, [], Members, View, Cluster).

update_view([], Errors, _Members, View, _Cluster) ->
    {lists:reverse(Errors), lists:usort(View)};
update_view([{add, Id}|Rest], Errors, Members, View, Cluster) ->
    InCluster = in_cluster(Id, Cluster),
    IsMember = lists:member(Id, Members),
    if not InCluster ->
            update_view(Rest, [{not_in_cluster, Id}|Errors], Members, View, Cluster);
       IsMember ->
            update_view(Rest, [{already_member, Id}|Errors], Members, View, Cluster);
       true ->
            update_view(Rest, Errors, [Id|Members], [Id|View], Cluster)
    end;
update_view([{del, Id}|Rest], Errors, Members, View, Cluster) ->
    case lists:member(Id, Members) of
        false ->
            update_view(Rest, [{not_member, Id}|Errors], Members, View, Cluster);
        true ->
            update_view(Rest, Errors, Members -- [Id], View -- [Id], Cluster)
    end.
```

## try_commit will change last_views

``` erlang
-spec try_commit(fact(), state()) -> {failed, state()} | {ok, state()}.
try_commit(NewFact0, State) ->
    Views = views(State),
    NewFact = increment_sequence(NewFact0),
    State2 = local_commit(NewFact, State),
    {Future, State3} = blocking_send_all({commit, NewFact}, State2),
    case wait_for_quorum(Future) of
        {quorum_met, _Replies} ->
            State4 = State3#state{last_views=Views},
            {ok, State4};
        {timeout, _Replies} ->
            {failed, set_leader(undefined, State3)}
    end.
```


## transition can update view in the final

``` erlang
-spec should_transition(state()) -> boolean().
should_transition(State=#state{last_views=LastViews}) ->
    Views = views(State),
    (Views =:= LastViews) and (tl(views(State)) =/= []).

-spec transition(state()) -> {ok, state()}       |
                             {shutdown, state()} |
                             {failed, state()}.
transition(State=#state{id=Id, fact=Fact}) ->
    Latest = hd(Fact#fact.views),
    ViewVsn = {Fact#fact.epoch, Fact#fact.seq},
    PendVsn = Fact#fact.pend_vsn,
    NewFact = Fact#fact{views=[Latest], view_vsn=ViewVsn, commit_vsn=PendVsn},
    case try_commit(NewFact, State) of
        {ok, State3} ->
            case lists:member(Id, Latest) of
                false ->
                    {shutdown, State3};
                true ->
                    {ok, State3}
            end;
        {failed, _}=Failed ->
            Failed
    end.
```


## two stable state

The leader state is the most update state of the group process, and following state is a copy of the leader state, and the following state exchange data within a `FOLLOWER_TIMEOUT` constant.
If the following exchange data failed, it change to probe state and start to election.
