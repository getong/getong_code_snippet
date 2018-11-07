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
