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