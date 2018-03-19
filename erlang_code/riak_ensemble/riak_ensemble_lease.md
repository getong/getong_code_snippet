# riak_ensemble lease

## Every ensemble has one lease process

``` erlang
setup({init, Args}, State0=#state{id=Id, ensemble=Ensemble, ets=ETS, mod=Mod}) ->
    ...
    {ok, Lease} = riak_ensemble_lease:start_link(),
    State = State0#state{workers=list_to_tuple(Workers),
        tree=Tree,
        fact=Saved,
        members=Members,
        lease=Lease,
        modstate=riak_ensemble_backend:start(Mod, Ensemble, Id, Args)},
    ...
```

## lease info
   The `Lease` info in the above, is {Pid, EtsTableId}

``` erlang
-spec start_link() -> {ok, lease_ref()}.
start_link() ->
    Ref = make_ref(),
    spawn_link(?MODULE, init, [self(), Ref]),
    receive
        {Ref, Reply} ->
            Reply
    end.

init(Parent, Ref) ->
    T = ets:new(?MODULE, [protected, set, {read_concurrency, true}]),
    ets:insert(T, {lease, undefined}),
    Reply = {ok, {self(), T}},
    Parent ! {Ref, Reply},
    loop(T, infinity).
```

## lease , unlease and check_lease operations
lease for a period of time, and unlease close the lease operation.
The check_lease will check the lease time is open for now.
