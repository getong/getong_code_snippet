# erlang-libp2p

## erlang-libp2p is a library application

## start example
``` erlang
SwarmOpts = [{libp2p_transport_tcp, [{nat, false}]}],
Version = “relaytest/1.0.0”,
{ok, ASwarm} = libp2p_swarm:start(a, SwarmOpts),
ok = libp2p_swarm:listen(ASwarm, “/ip4/0.0.0.0/tcp/6602”),
libp2p_swarm:add_stream_handler(
    ASwarm
    ,Version
    ,{libp2p_framed_stream, server, [libp2p_stream_relay_test, self(), ASwarm]}
).

 ```
 copy from [relay.md](https://github.com/helium/erlang-libp2p/blob/master/doc/relay.md)

 ## start transport

``` erlang
start_link(TransportMod, TID) ->
    case TransportMod:start_link(TID) of
        {ok, TransportPid} ->
            libp2p_config:insert_transport(TID, TransportMod, TransportPid),
            %% on bootup we're blocking the top level supervisor's init, so we need to
            %% call back asynchronously
            spawn(fun() ->
                          Server = libp2p_swarm_sup:server(libp2p_swarm:swarm(TID)),
                          gen_server:cast(Server, {register, libp2p_config:transport(), TransportPid})
                  end),
            {ok, TransportPid};
        ignore ->
            %% for some reason we register these transports as `undefined`
            libp2p_config:insert_transport(TID, TransportMod, undefined),
            ignore;
        Other ->
            Other
    end.

```
It store the pid into a ets table, and register the pid to the server.

## mk_async_sender

``` erlang
-spec mk_async_sender(pid(), libp2p_connection:connection()) -> fun().
mk_async_sender(Handler, Connection) ->
    Parent = self(),
    Sender = fun Fun() ->
                     receive
                         {'DOWN', _, process, Parent, _} ->
                             ok;
                         {send, Ref, Data} ->
                             case (catch libp2p_connection:send(Connection, Data)) of
                                 {'EXIT', Error} ->
                                     lager:notice("Failed sending on connection for ~p: ~p",
                                                  [Handler, Error]),
                                     Handler ! {send_result, Ref, {error, Error}};
                                 Result ->
                                     Handler ! {send_result, Ref, Result}
                             end,
                             Fun();
                         {cast, Data} ->
                             case (catch libp2p_connection:send(Connection, Data)) of
                                 {'EXIT', Error} ->
                                     lager:notice("Failed casting on connection for ~p: ~p",
                                                  [Handler, Error]);
                                 _ ->
                                     ok
                             end,
                             Fun()
                     end
             end,
    fun() ->
            erlang:monitor(process, Parent),
            Sender()
    end.
```
in the libp2p_framed_stream.erl module, it call the function:

``` erlang
SendPid = spawn_link(libp2p_connection:mk_async_sender(self(), Connection)),
```
and send the data :

``` erlang
-spec handle_resp_send(send_result_action(), binary(), non_neg_integer(), #state{}) -> #state{}.
handle_resp_send(Action, Data, Timeout, State=#state{sends=Sends, send_pid=SendPid}) ->
    Key = make_ref(),
    Timer = erlang:send_after(Timeout, self(), {send_result, Key, {error, timeout}}),
    Bin = <<(byte_size(Data)):32/little-unsigned-integer, Data/binary>>,
    SendPid ! {send, Key, Bin},
    State#state{sends=maps:put(Key, {Timer, Action}, Sends)}.
```
and handle the send result:

``` erlang
handle_info({send_result, Key, Result}, State=#state{sends=Sends}) ->
    case maps:take(Key, Sends) of
        error -> {noreply, State};
        {{Timer, Info}, NewSends} ->
            erlang:cancel_timer(Timer),
            handle_send_result(Info, Result, State#state{sends=NewSends})
    end;


-spec handle_send_result(send_result_action(), ok | {error, term()}, #state{}) ->
                                {noreply, #state{}} |
                                {stop, Reason::term(), #state{}}.
handle_send_result({reply, From}, Result, State=#state{}) ->
    gen_server:reply(From, Result),
    {noreply, State};
handle_send_result({reply, From, Reply}, ok, State=#state{}) ->
    gen_server:reply(From, Reply),
    {noreply, State};
handle_send_result({reply, From, Reply}, {error, closed}, State=#state{}) ->
    gen_server:reply(From, Reply),
    {stop, normal, State};
handle_send_result(noreply, ok, State=#state{}) ->
    {noreply, State};
handle_send_result({stop, Reason}, ok, State=#state{}) ->
    {stop, Reason, State};
handle_send_result({stop, Reason, From, Reply}, ok, State=#state{}) ->
    gen_server:reply(From, Reply),
    {stop, Reason, State};
handle_send_result({stop, Reason, From, Reply}, {error, closed}, State=#state{}) ->
    gen_server:reply(From, Reply),
    {stop, Reason, State};
handle_send_result(_, {error, timeout}, State=#state{}) ->
    {stop, normal, State};
handle_send_result(_, {error, closed}, State=#state{}) ->
    {stop, normal, State};
handle_send_result(_, {error, Error}, State=#state{})  ->
    {stop, {error, Error}, State}.
```
Every send data, will get a ref, and if the timeout, it will notice it at once.

## the ets table

``` erlang
TID = ets:new(Name, [public, ordered_set, {read_concurrency, true}]),
ets:insert(TID, {?SUP, self()}),
ets:insert(TID, {?NAME, Name}),
ets:insert(TID, {?OPTS, Opts}),
```
copy from libp2p_swarm_sup.erl.
All the process start with this `TID` and all options are stored in this ets table.
