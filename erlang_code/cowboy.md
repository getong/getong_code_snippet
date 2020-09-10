# cowboy

## start_http_server

``` erlang
start_http_server() ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/",                 cowboy_static, {priv_file, mzbench_api, "/http_root/index.html"}},
            {"/dev",              cowboy_static, {priv_file, mzbench_api, "/http_root/index.dev.html"}},
            {"/js/vendors/[...]", cowboy_static, {priv_dir, mzbench_api, ["/http_root/js/vendors"], [{mimetypes, cow_mimetypes, web}]}},
            {"/js/[...]",         cowboy_static, {priv_dir, mzbench_api, ["/http_root/js"], [{mimetypes, cow_mimetypes, web}]}},
            {"/css/[...]",        cowboy_static, {priv_dir, mzbench_api, ["/http_root/css"], [{mimetypes, cow_mimetypes, web}]}},
            {"/favicon.ico",      cowboy_static, {priv_file, mzbench_api, "/http_root/favicon.ico"}},
            {"/ws",               mzb_api_ws_handler, []},
            {'_',                 mzb_api_endpoints, []}
        ]}
    ]),
    {ok, CowboyInterfaceStr} = application:get_env(mzbench_api, network_interface),
    {ok, CowboyInterface} = inet_parse:address(CowboyInterfaceStr),
    {ok, CowboyPort} = application:get_env(mzbench_api, listen_port),
    {ok, Protocol} =  application:get_env(mzbench_api, protocol),
    lager:info("Starting cowboy ~p listener on ~p:~p", [Protocol, CowboyInterface, CowboyPort]),
    Params = [{port, CowboyPort}, {ip, CowboyInterface}],
    Env = [{env, [{dispatch, Dispatch}]}],
    {ok, _} = case Protocol of
        http -> cowboy:start_http(http, 100, Params, Env);
        https ->
            {ok, CertFile} = application:get_env(mzbench_api, certfile),
            {ok, KeyFile} = application:get_env(mzbench_api, keyfile),
            CACertInList =  case application:get_env(mzbench_api, certfile, none) of
                                none -> [];
                                F -> [{cacertfile, mzb_file:expand_filename(F)}]
                            end,
            cowboy:start_https(https, 100, Params ++ CACertInList
                                ++ [{certfile, mzb_file:expand_filename(CertFile)},
                                    {keyfile, mzb_file:expand_filename(KeyFile)}], Env)
        end,
    ok.
```

copy from mzb_api_app.erl

``` erlang
Dispatch = cowboy_router:compile([
        {'_', [{"/", hello_handler, []}]}
    ]),
    {ok, _} = cowboy:start_clear(my_http_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),
```
>> For this tutorial we map the path / to the handler module hello_handler.
copy from [Nine Nines: Getting started](https://ninenines.eu/docs/en/cowboy/2.2/guide/getting_started/)

## cowboy rest
The restful way implemented in cowboy is cowboy_rest.erl, and many callback functions can be not implemented.


## cowboy start spec

``` erlang
Dispatch = cowboy_router:compile([
    %% {HostMatch, list({PathMatch, Handler, InitialState})}
    {'_', [{'_', my_handler, #{}}]}
]),
%% Name, NbAcceptors, TransOpts, ProtoOpts
cowboy:start_clear(my_http_listener,
    [{port, 8080}],
    #{env => #{dispatch => Dispatch}}
).
```
copy from [Nine Nines: Routing](https://ninenines.eu/docs/en/cowboy/2.2/guide/routing/)

## live update

``` erlang
Dispatch = cowboy_router:compile(Routes),
cowboy:set_env(my_http_listener, dispatch, Dispatch).
```
copy from [Nine Nines: Routing](https://ninenines.eu/docs/en/cowboy/2.2/guide/routing/)


## parse post data

application/x-www-form-urlencoded

``` shell
curl -d "abc=abc" http://localhost/order
```
cowboy code
``` erlang
{ok, KeyValues, Req} = cowboy_req:read_urlencoded_body(Req0).
```
application/json

``` shell
curl -H "Content-Type: application/json" -X POST -d '{"username":"xyz","password":"xyz"}' http://127.0.0.1
```

cowboy code

``` erlang
{ok, Data, Req} = cowboy_req:read_body(Req0),
jiffy:decode(Data).
```

## Resource design

```
If you implement the methods GET and/or HEAD, you must implement one ProvideResource callback for each content-type returned by the content_types_provided callback.
If you implement the methods PUT, POST and/or PATCH, you must implement the content_types_accepted callback, and one AcceptCallback callback for each content-type it returns.
If you implement the method DELETE, you must implement the delete_resource callback.
```
copy from [Designing a resource handler](https://ninenines.eu/docs/en/cowboy/2.2/guide/resource_design/)

## websocket

``` erlang
%% The init/2 callback is called when the request is received.
%% To establish a Websocket connection, you must switch to the cowboy_websocket module:
init(Req, State) ->
    {cowboy_websocket, Req, State}.

%% Cowboy will call websocket_handle/2 whenever a text, binary, ping or pong frame arrives from the client.
websocket_handle(Frame = {text, _}, State) ->
    {reply, Frame, State};
websocket_handle(_Frame, State) ->
    {ok, State}.

%% Cowboy will call websocket_info/2 whenever an Erlang message arrives.
websocket_info({log, Text}, State) ->
    {reply, {text, Text}, State};
websocket_info(_Info, State) ->
    {ok, State}.
```

More info see [Websocket handlers](https://ninenines.eu/docs/en/cowboy/2.5/guide/ws_handlers/)

## gun, the erlang websocket client
see [How do you use Gun as a Cowboy client?](https://stackoverflow.com/questions/45005984/how-do-you-use-gun-as-a-cowboy-client)
[Websocket](https://ninenines.eu/docs/en/gun/1.3/guide/websocket/)

## cowboy_http.erl main loop funcion

``` erlang
loop(State=#state{parent=Parent, socket=Socket, transport=Transport, opts=Opts,
		timer=TimerRef, children=Children, in_streamid=InStreamID,
		last_streamid=LastStreamID, streams=Streams}, Buffer) ->
```
It has two arguments, one is the current `State`, the other is the `Buffer`.
The cowboy_http.erl uses active once mode, the before_loop function set the mode every time.

``` erlang
before_loop(State=#state{socket=Socket, transport=Transport}, Buffer) ->
	%% @todo disable this when we get to the body, until the stream asks for it?
	%% Perhaps have a threshold for how much we're willing to read before waiting.
	Transport:setopts(Socket, [{active, once}]),
	loop(State, Buffer).
```
## cowboy http module, the in_state represents the current http state

``` erlang
parse(<<>>, State) ->
	before_loop(State, <<>>);
%% Do not process requests that come in after the last request
%% and discard the buffer if any to save memory.
parse(_, State=#state{in_streamid=InStreamID, in_state=#ps_request_line{},
		last_streamid=LastStreamID}) when InStreamID > LastStreamID ->
	before_loop(State, <<>>);
parse(Buffer, State=#state{in_state=#ps_request_line{empty_lines=EmptyLines}}) ->
	after_parse(parse_request(Buffer, State, EmptyLines));
parse(Buffer, State=#state{in_state=PS=#ps_header{headers=Headers, name=undefined}}) ->
	after_parse(parse_header(Buffer,
		State#state{in_state=PS#ps_header{headers=undefined}},
		Headers));
parse(Buffer, State=#state{in_state=PS=#ps_header{headers=Headers, name=Name}}) ->
	after_parse(parse_hd_before_value(Buffer,
		State#state{in_state=PS#ps_header{headers=undefined, name=undefined}},
		Headers, Name));
parse(Buffer, State=#state{in_state=#ps_body{}}) ->
	%% @todo We do not want to get the body automatically if the request doesn't ask for it.
	%% We may want to get bodies that are below a threshold without waiting, and buffer them
	%% until the request asks, though.
	after_parse(parse_body(Buffer, State)).

before_parse_headers(Rest, State, M, A, P, Q, V) ->
	parse_header(Rest, State#state{in_state=#ps_header{
		method=M, authority=A, path=P, qs=Q, version=V}}, #{}).
```

## cowboy reset
The callback and optional_callbacks attributes

``` erlang
-callback delete_completed(Req, State)
	-> {boolean(), Req, State}
	| {stop, Req, State}
	| {switch_handler(), Req, State}
	when Req::cowboy_req:req(), State::any().
-optional_callbacks([delete_completed/2]).
```
And the function implement:

``` erlang
%% delete_completed/2 indicates whether the resource has been deleted yet.
delete_completed(Req, State) ->
	expect(Req, State, delete_completed, true, fun has_resp_body/2, 202).

expect(Req, State, Callback, Expected, OnTrue, OnFalse) ->
	case call(Req, State, Callback) of
		no_call ->
			next(Req, State, OnTrue);
		{stop, Req2, HandlerState} ->
			terminate(Req2, State#state{handler_state=HandlerState});
		{Switch, Req2, HandlerState} when element(1, Switch) =:= switch_handler ->
			switch_handler(Switch, Req2, HandlerState);
		{Expected, Req2, HandlerState} ->
			next(Req2, State#state{handler_state=HandlerState}, OnTrue);
		{_Unexpected, Req2, HandlerState} ->
			next(Req2, State#state{handler_state=HandlerState}, OnFalse)
	end.

call(Req, State=#state{handler=Handler, handler_state=HandlerState},
		Callback) ->
	case erlang:function_exported(Handler, Callback, 2) of
		true ->
			try
				Handler:Callback(Req, HandlerState)
			catch Class:Reason ->
				error_terminate(Req, State, Class, Reason)
			end;
		false ->
			no_call
	end.

next(Req, State, Next) when is_function(Next) ->
	Next(Req, State);
next(Req, State, StatusCode) when is_integer(StatusCode) ->
	respond(Req, State, StatusCode).

respond(Req, State, StatusCode) ->
	terminate(cowboy_req:reply(StatusCode, Req), State).
```
The `expect` function will check the calback module `function_exported` the `Callback` function exist, if not, Call the `Next` function or terminate function.

## stop_listener

``` erlang
-spec stop_listener(ranch:ref()) -> ok | {error, not_found}.
stop_listener(Ref) ->
	ranch:stop_listener(Ref).
```

## cowboy cors
look at [Add CORS example](https://github.com/ninenines/cowboy/pull/1001)


## cowboy ssl configuration

``` erlang
{ok, CowboyPid} = cowboy:start_https(
    cowboy_https_receiver,
    100,
    [
        {port, 443},
        {cacertfile,"/path/to/testca/cacert.pem"},
        {certfile,"/path/to/server/cert.pem"},
        {keyfile,"/path/to/server/key.pem"},
        {versions, ['tlsv1.2', 'tlsv1.1', 'tlsv1']},
        {dhfile, "/path/to/testca/dh-params.pem"},
        {ciphers, ["ECDHE-ECDSA-AES256-GCM-SHA384","ECDHE-RSA-AES256-GCM-SHA384",
            "ECDHE-ECDSA-AES256-SHA384","ECDHE-RSA-AES256-SHA384", "ECDHE-ECDSA-DES-CBC3-SHA",
            "ECDH-ECDSA-AES256-GCM-SHA384","ECDH-RSA-AES256-GCM-SHA384","ECDH-ECDSA-AES256-SHA384",
            "ECDH-RSA-AES256-SHA384","DHE-DSS-AES256-GCM-SHA384","DHE-DSS-AES256-SHA256",
            "AES256-GCM-SHA384","AES256-SHA256","ECDHE-ECDSA-AES128-GCM-SHA256",
            "ECDHE-RSA-AES128-GCM-SHA256","ECDHE-ECDSA-AES128-SHA256","ECDHE-RSA-AES128-SHA256",
            "ECDH-ECDSA-AES128-GCM-SHA256","ECDH-RSA-AES128-GCM-SHA256","ECDH-ECDSA-AES128-SHA256",
            "ECDH-RSA-AES128-SHA256","DHE-DSS-AES128-GCM-SHA256","DHE-DSS-AES128-SHA256",
            "AES128-GCM-SHA256","AES128-SHA256","ECDHE-ECDSA-AES256-SHA",
            "ECDHE-RSA-AES256-SHA","DHE-DSS-AES256-SHA","ECDH-ECDSA-AES256-SHA",
            "ECDH-RSA-AES256-SHA","AES256-SHA","ECDHE-ECDSA-AES128-SHA",
            "ECDHE-RSA-AES128-SHA","DHE-DSS-AES128-SHA","ECDH-ECDSA-AES128-SHA",
            "ECDH-RSA-AES128-SHA","AES128-SHA"]},
        {secure_renegotiate, true},
        {reuse_sessions, true},
        {honor_cipher_order, true},
        {max_connections, infinity}
    ],
    [{max_keepalive, 1024}, {env, [{dispatch, Dispatch}]}]).
```

phoniex ssl configuration
config/prod.exs
``` elixir
use Mix.Config

config :phoenix, SSLApp.Endpoint,
  https: [port: 443,
          host: 'sslapp.com',
          cacertfile: '/path/to/testca/cacert.pem',
          certfile: '/path/to/server/cert.pem',
          keyfile: '/path/to/server/key.pem',
          versions: [:'tlsv1.2', :'tlsv1.1', :'tlsv1'],
          dhfile: '/path/to/testca/dh-params.pem',
          ciphers: ['ECDHE-ECDSA-AES256-GCM-SHA384','ECDHE-RSA-AES256-GCM-SHA384',
                        'ECDHE-ECDSA-AES256-SHA384','ECDHE-RSA-AES256-SHA384', 'ECDHE-ECDSA-DES-CBC3-SHA',
                        'ECDH-ECDSA-AES256-GCM-SHA384','ECDH-RSA-AES256-GCM-SHA384','ECDH-ECDSA-AES256-SHA384',
                        'ECDH-RSA-AES256-SHA384','DHE-DSS-AES256-GCM-SHA384','DHE-DSS-AES256-SHA256',
                        'AES256-GCM-SHA384','AES256-SHA256','ECDHE-ECDSA-AES128-GCM-SHA256',
                        'ECDHE-RSA-AES128-GCM-SHA256','ECDHE-ECDSA-AES128-SHA256','ECDHE-RSA-AES128-SHA256',
                        'ECDH-ECDSA-AES128-GCM-SHA256','ECDH-RSA-AES128-GCM-SHA256','ECDH-ECDSA-AES128-SHA256',
                        'ECDH-RSA-AES128-SHA256','DHE-DSS-AES128-GCM-SHA256','DHE-DSS-AES128-SHA256',
                        'AES128-GCM-SHA256','AES128-SHA256','ECDHE-ECDSA-AES256-SHA',
                        'ECDHE-RSA-AES256-SHA','DHE-DSS-AES256-SHA','ECDH-ECDSA-AES256-SHA',
                        'ECDH-RSA-AES256-SHA','AES256-SHA','ECDHE-ECDSA-AES128-SHA',
                        'ECDHE-RSA-AES128-SHA','DHE-DSS-AES128-SHA','ECDH-ECDSA-AES128-SHA',
                        'ECDH-RSA-AES128-SHA','AES128-SHA'],
          secure_renegotiate: true,
          reuse_sessions: true,
          honor_cipher_order: true,
          max_connections: :infinity]
```

copy from [Increasing security in Erlang and Elixir SSL applications](http://ezgr.net/increasing-security-erlang-ssl-cowboy/)
