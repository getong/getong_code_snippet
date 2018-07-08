# partisan

## plumtree broadcast
divide nodes into two parts and broadcast message
``` erlang
handle_cast({broadcast, MessageId, Message, Mod}, State) ->
    ?UTIL:log(debug, "received {broadcast, ~p, Msg, ~p}",
                      [MessageId, Mod]),
    State1 = eager_push(MessageId, Message, Mod, State),
    State2 = schedule_lazy_push(MessageId, Mod, State1),
    {noreply, State2};
handle_cast({broadcast, MessageId, Message, Mod, Round, Root, From}, State) ->
    ?UTIL:log(debug, "received {broadcast, ~p, Msg, ~p, ~p, ~p, ~p}",
                      [MessageId, Mod, Round, Root, From]),
    Valid = Mod:merge(MessageId, Message),
    State1 = handle_broadcast(Valid, MessageId, Message, Mod, Round, Root, From, State),
    {noreply, State1};
```

## node name
use uuid as node name
``` erlang
Name = case node() of
        nonode@nohost ->
            lager:info("Distributed Erlang is not enabled, generating UUID."),
            UUIDState = uuid:new(self()),
            {UUID, _UUIDState1} = uuid:get_v1(UUIDState),
            lager:info("Generated UUID: ~p, converting to string.", [UUID]),
            StringUUID = uuid:uuid_to_string(UUID),
            NodeName = list_to_atom(StringUUID ++ "@127.0.0.1"),
            lager:info("Generated name for node: ~p", [NodeName]),
            NodeName;
        Other ->
            lager:info("Using node name: ~p", [Other]),
            Other
    end,
```
