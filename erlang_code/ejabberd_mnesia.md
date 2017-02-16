#ejabberd acl mnesia operation
## add operation

```
%% copy from acl.erl
-spec add(binary(), aclname(), aclspec()) -> ok | {error, any()}.

add(Host, ACLName, ACLSpec) ->
    {ResL, BadNodes} = ejabberd_cluster:multicall(
                                     ?MODULE, add_local,
                                     [Host, ACLName, ACLSpec]),
    case lists:keyfind(aborted, 1, ResL) of
        false when BadNodes == [] ->
            ok;
        false ->
            {error, {failed_nodes, BadNodes}};
        Err ->
            {error, Err}
    end.

add_local(Host, ACLName, ACLSpec) ->
    F = fun () ->
		mnesia:write(#acl{aclname = {ACLName, Host},
				  aclspec = normalize_spec(ACLSpec)})
	end,
    case mnesia:transaction(F) of
        {atomic, ok} ->
            ok;
        Err ->
            Err
    end.

```
the multicall/3 function defines:

```
%% copy from ejabberd_cluster.erl
-spec multicall(module(), atom(), [any()]) -> {list(), [node()]}.

multicall(Module, Function, Args) ->
    multicall(get_nodes(), Module, Function, Args).

-spec multicall([node()], module(), atom(), list()) -> {list(), [node()]}.

multicall(Nodes, Module, Function, Args) ->
    rpc:multicall(Nodes, Module, Function, Args, 5000).

-spec get_nodes() -> [node()].

get_nodes() ->
    mnesia:system_info(running_db_nodes).
```
通过mnesia获取所有节点名称，然后通过rpc调用函数.值得注意的是rpc调用的函数里面使用了事务。

## 脏读获取信息

```
%% copy from acl.erl
get_aclspecs(ACL, Host) ->
    mnesia:dirty_read(acl, {ACL, Host}) ++ mnesia:dirty_read(acl, {ACL, global}).
```
