# erlang supervisor

## simple_one_for_one

copy from esockd_acceptor_sup.erl
``` erlang
%% @doc Start a acceptor.
-spec(start_acceptor(AcceptorSup, LSock, SockFun) -> {ok, pid()} | {error, any()} | ignore when
      AcceptorSup :: pid(),
      LSock       :: inet:socket(),
      SockFun     :: esockd:sock_fun()).
start_acceptor(AcceptorSup, LSock, SockFun) ->
    supervisor:start_child(AcceptorSup, [LSock, SockFun]).


init([ConnSup, AcceptStatsFun, BufferTuneFun, Logger]) ->
    {ok, {{simple_one_for_one, 1000, 3600},
          [{acceptor, {esockd_acceptor, start_link, [ConnSup, AcceptStatsFun, BufferTuneFun, Logger]},
            transient, 5000, worker, [esockd_acceptor]}]}}.
```

In the esockd_acceptor.erl module
```
%% @doc Start Acceptor
-spec(start_link(ConnSup, AcceptStatsFun, BufferTuneFun, Logger, LSock, SockFun) -> {ok, pid()} | {error, any()} when
      ConnSup        :: pid(),
      AcceptStatsFun :: fun(),
      BufferTuneFun  :: esockd:tune_fun(),
      Logger         :: gen_logger:logmod(),
      LSock          :: inet:socket(),
      SockFun        :: esockd:sock_fun()).
start_link(ConnSup, AcceptStatsFun, BufTuneFun, Logger, LSock, SockFun) ->
    gen_server:start_link(?MODULE, {ConnSup, AcceptStatsFun, BufTuneFun, Logger, LSock, SockFun}, []).

init({ConnSup, AcceptStatsFun, BufferTuneFun, Logger, LSock, SockFun}) ->
    {ok, SockName} = inet:sockname(LSock),
    gen_server:cast(self(), accept),
    {ok, #state{lsock    = LSock,
                sockfun  = SockFun,
                tunefun  = BufferTuneFun,
                sockname = esockd_net:format(sockname, SockName),
                conn_sup = ConnSup,
                statsfun = AcceptStatsFun,
                logger   = Logger}}.
```
The start_link function has 6 elements, and in the esockd_acceptor_sup module, the elements are divided into two parts.
Look into the supervisor.erl module:

``` erlang
handle_call({start_child, EArgs}, _From, State) when ?is_simple(State) ->
    Child = hd(State#state.children),
    #child{mfargs = {M, F, A}} = Child,
    Args = A ++ EArgs,
    case do_start_child_i(M, F, Args) of
    {ok, undefined} ->
        {reply, {ok, undefined}, State};
    {ok, Pid} ->
        NState = save_dynamic_child(Child#child.restart_type, Pid, Args, State),
        {reply, {ok, Pid}, NState};
    {ok, Pid, Extra} ->
        NState = save_dynamic_child(Child#child.restart_type, Pid, Args, State),
        {reply, {ok, Pid, Extra}, NState};
    What ->
        {reply, What, State}
    end;

```
The two parts element are plused in one list. That is it.
