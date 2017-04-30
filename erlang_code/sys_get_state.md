#sys get_state
##in sys.erl
```
get_state(Name) ->
    case send_system_msg(Name, get_state) of
	{error, Reason} -> error(Reason);
	State -> State
    end.

send_system_msg(Name, Request) ->
    case catch gen:call(Name, system, Request) of
	{ok,Res} -> Res;
	{'EXIT', Reason} -> exit({Reason, mfa(Name, Request)})
    end.

send_system_msg(Name, Request, Timeout) ->
    case catch gen:call(Name, system, Request, Timeout) of
	{ok,Res} -> Res;
	{'EXIT', Reason} -> exit({Reason, mfa(Name, Request, Timeout)})
    end.

do_cmd(SysState, get_state, _Parent, Mod, Debug, Misc) ->
    {SysState, do_get_state(Mod, Misc), Debug, Misc};

do_get_state(Mod, Misc) ->
    case erlang:function_exported(Mod, system_get_state, 1) of
	true ->
	    try
		{ok, State} = Mod:system_get_state(Misc),
		State
	    catch
		Cl:Exc ->
		    {error, {callback_failed,{Mod,system_get_state},{Cl,Exc}}}
	    end;
	false ->
	    Misc
    end.


```

## in gen_fsm.erl
```
system_get_state([_Name, StateName, StateData, _Mod, _Time]) ->
    {ok, {StateName, StateData}}.
```
## in gen_server.erl

```
system_get_state([_Name, State, _Mod, _Time]) ->
    {ok, State}.
```

## sys trace

``` erlang
sys:trace(Name, Flag)
```
Trace `Name` process message.
`Name` process can be pid, atom(register atom name)
