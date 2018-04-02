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
