# ejabberd hooks
ejabberd_hooks.erl support add/2,3,4, add_dist/5,6, delete/4,5,6 and get_handlers/2 operations. They are all call to the `ejabberd_hooks` process, and then the process add the info to the ets table.
for example, like the add function:

```
add(Hook, Host, Module, Function, Seq) ->
    gen_server:call(ejabberd_hooks, {add, Hook, Host, Module, Function, Seq}).

handle_call({add, Hook, Host, Module, Function, Seq}, _From, State) ->
    HookFormat = {Seq, Module, Function},
    Reply = handle_add(Hook, Host, HookFormat),
    {reply, Reply, State};
handle_call({add, Hook, Host, Node, Module, Function, Seq}, _From, State) ->
    HookFormat = {Seq, Node, Module, Function},
    Reply = handle_add(Hook, Host, HookFormat),
    {reply, Reply, State};

-spec handle_add(atom(), atom(), local_hook() | distributed_hook()) -> ok.
%% in-memory storage operation: Handle adding hook in ETS table
handle_add(Hook, Host, El) ->
    case ets:lookup(hooks, {Hook, Host}) of
        [{_, Ls}] ->
            case lists:member(El, Ls) of
                true ->
                    ok;
                false ->
                    NewLs = lists:merge(Ls, [El]),
                    ets:insert(hooks, {{Hook, Host}, NewLs}),
                    ok
            end;
        [] ->
            NewLs = [El],
            ets:insert(hooks, {{Hook, Host}, NewLs}),
            ok
    end.
```
Notice the `Seq` parameter, it defines the sequence which function runs.
The ets stores the info used the `Host` as the key, the functions info is the value.
`lists:merge/2` function merge two list according to the tuple.
## ejabberd_hooks supports the run/2,3, and run_fold/3,4 operations.
The `run` get the functions via the `Host` and call the `ejabberd_cluster:call/3` to achieve the goal.
The `run_fold` is almost the same with `run` function, but it can like `list:foldl` function to accumulate the result.

## ejabberd_hooks start in every ejabberd node, it only control `Host` in the local node. So it often used with `ejabberd_router:register_route`.
for example:

```
%% copy from ejabberd_lcoal.erl
init([]) ->
    lists:foreach(fun (Host) ->
			  ejabberd_router:register_route(Host,
							 Host,
							 {apply, ?MODULE,
							  route}),
			  ejabberd_hooks:add(local_send_to_resource_hook, Host,
					     ?MODULE, bounce_resource_packet,
					     100)
		  end,
		  ?MYHOSTS),
    catch ets:new(?IQTABLE, [named_table, public]),
    update_table(),
    ejabberd_mnesia:create(?MODULE, iq_response,
			[{ram_copies, [node()]},
			 {attributes, record_info(fields, iq_response)}]),
    mnesia:add_table_copy(iq_response, node(), ram_copies),
    {ok, #state{}}.
```
And also:

```
%% copy from ejabberd_sm.erl
init([]) ->
    lists:foreach(fun(Mod) -> Mod:init() end, get_sm_backends()),
    ets:new(sm_iqtable, [named_table]),
    lists:foreach(
      fun(Host) ->
	      ejabberd_hooks:add(roster_in_subscription, Host,
				 ejabberd_sm, check_in_subscription, 20),
	      ejabberd_hooks:add(offline_message_hook, Host,
				 ejabberd_sm, bounce_offline_message, 100),
	      ejabberd_hooks:add(remove_user, Host,
				 ejabberd_sm, disconnect_removed_user, 100)
      end, ?MYHOSTS),
    ejabberd_commands:register_commands(get_commands_spec()),
    {ok, #state{}}.
```
So it often be used in the plugin modules.

## local Node and remote Node
If Node is not specified, it calls the functions in local Node.
If Node is specified, it calls the `ejabberd_cluster:call`. The `ejabberd_cluster:call` just calls `rpc:call` with 5000 milliseconds timeout.

## safe_apply
It support two types of function,  one is fun function, the other is MFA function.
So is the safe_apply function.

```
safe_apply(Module, Function, Args) ->
    if is_function(Function) ->
            catch apply(Function, Args);
       true ->
            catch apply(Module, Function, Args)
    end.
```
