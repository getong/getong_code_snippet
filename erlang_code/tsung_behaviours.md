# tsung behaviours
## gen_ts_transport
`gen_ts_transport` is the transport library, it describes how the network works.
The behaviour_info is :

```
behaviour_info(callbacks) ->
    [{connect, 4},
     {send, 3},
     {close, 1},
     {set_opts, 2},
     {protocol_options, 1},
     {normalize_incomming_data, 2}];

behaviour_info(_Other) ->
    undefined.
```

The supported modules are:

```
ts_bosh.erl
ts_bosh_ssl.erl
ts_erlang.erl
ts_server_websocket.erl
ts_ssl6.erl
ts_ssl.erl
ts_tcp6.erl
ts_tcp.erl
ts_udp6.erl
ts_udp.erl
```

## ts_plugin
`ts_plugin` is the application that is to be tested. It acts like a client of the application.
The behaviour_info is :

```
behaviour_info(callbacks) ->
    [{add_dynparams, 4},
     {get_message, 2},
     {session_defaults, 0},
     {dump, 2},
     {parse, 2},
     {parse_bidi, 2},
     {parse_config, 2},
     {decode_buffer, 2},
     {new_session, 0}];
behaviour_info(_Other) ->
    undefined.
```

The supported modules are:
```
ts_amqp.erl
ts_fs.erl
ts_http.erl
ts_jabber.erl
ts_job.erl
ts_ldap.erl
ts_mqtt.erl
ts_mysql.erl
ts_pgsql.erl
ts_raw.erl
ts_shell.erl
ts_webdav.erl
ts_websocket.erl
```
