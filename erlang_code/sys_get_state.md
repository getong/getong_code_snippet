#sys get_state

## The doc
>This module contains functions for sending system messages used by programs, and messages used for debugging purposes.
>
>Functions  used for implementation of processes are also expected to understand system messages, such as debug messages
>and code change. These functions must be used to implement the use of system messages for a process;  either  directly,
>or through standard behaviors, such as gen_server.
>
>The  default  time-out  is  5000  ms,  unless  otherwise specified. timeout defines the time to wait for the process to
>respond to a request. If the process does not respond, the function evaluates exit({timeout, {M, F, A}}).
>
>The functions make references to a debug structure. The debug structure is a list of dbg_opt(), which  is  an  internal
>data type used by function handle_system_msg/6. No debugging is performed if it is an empty list.

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
