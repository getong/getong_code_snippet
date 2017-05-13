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
