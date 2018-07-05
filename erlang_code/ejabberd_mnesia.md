# ejabberd acl mnesia operation
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

## mnesia:dirty_update_counter
Copy from `ejabberd_auth_mnesia.erl`
```
try_register(User, Server, PasswordList) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    Password = if is_list(PasswordList); is_binary(PasswordList) ->
      iolist_to_binary(PasswordList);
      true -> PasswordList
    end,
    LPassword = jid:resourceprep(Password),
    US = {LUser, LServer},
    if (LUser == error) or (LServer == error) ->
	   {error, invalid_jid};
       (LPassword == error) and not is_record(Password, scram) ->
	   {error, invalid_password};
       true ->
	   F = fun () ->
		       case mnesia:read({passwd, US}) of
			 [] ->
			     Password2 = case is_scrammed() and
						is_binary(Password)
					     of
					   true -> password_to_scram(Password);
					   false -> Password
					 end,
			     mnesia:write(#passwd{us = US,
						  password = Password2}),
			     mnesia:dirty_update_counter(reg_users_counter,
							 LServer, 1),
			     ok;
			 [_E] -> exists
		       end
	       end,
	   mnesia:transaction(F)
    end.


remove_user(User, Server) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    US = {LUser, LServer},
    F = fun () ->
		mnesia:delete({passwd, US}),
		mnesia:dirty_update_counter(reg_users_counter, LServer,
					    -1)
	end,
    mnesia:transaction(F),
    ok.

remove_user(User, Server, Password) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    US = {LUser, LServer},
    F = fun () ->
		case mnesia:read({passwd, US}) of
		  [#passwd{password = Password}]
		      when is_binary(Password) ->
		      mnesia:delete({passwd, US}),
		      mnesia:dirty_update_counter(reg_users_counter, LServer,
						  -1),
		      ok;
		  [#passwd{password = Scram}]
		      when is_record(Scram, scram) ->
		      case is_password_scram_valid(Password, Scram) of
			true ->
			    mnesia:delete({passwd, US}),
			    mnesia:dirty_update_counter(reg_users_counter,
							LServer, -1),
			    ok;
			false -> not_allowed
		      end;
		  _ -> not_exists
		end
	end,
    case mnesia:transaction(F) of
      {atomic, ok} -> ok;
      {atomic, Res} -> Res;
      _ -> bad_request
    end.
```

## set_master

``` erlang
set_master("self") ->
    set_master(node());
set_master(NodeString) when is_list(NodeString) ->
    set_master(list_to_atom(NodeString));
set_master(Node) when is_atom(Node) ->
    case mnesia:set_master_nodes([Node]) of
        ok ->
	    {ok, ""};
	{error, Reason} ->
	    String = io_lib:format("Can't set master node ~p at node ~p:~n~p",
				   [Node, node(), Reason]),
	    {error, String}
    end.
```

## ets:update_counter and mnesia:dirty_update_counter

``` erlang
%% ets
Table = ets:new(x, []).
ets:insert(Table, {x, null}).
ets:update_counter(Table, x, 1).
** exception error: bad argument
     in function  ets:update_counter/3
	         called as ets:update_counter(#Ref<0.1560783866.2387214337.254665>,x,1)


%% mnesia
mnesia:start().
rd(table, {a,b}).
mnesia:create_table(table, [{ram_copies, [node()]}, {type, set}, {attributes, record_info(fields, table)}]).

mnesia:dirty_write({table, a, 2}).
mnesia:dirty_update_counter({table, a}, 1).

mnesia:dirty_write({table, b, null}).

mnesia:dirty_update_counter({table, b}, 1).
```

The mnesia output:

``` erlang
erl
Erlang/OTP 20 [erts-9.0.4] [source] [64-bit] [smp:4:4] [ds:4:4:10] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V9.0.4  (abort with ^G)
1> mnesia:start().
ok
2> rd(table, {a,b}).
table
3> mnesia:create_table(table, [{ram_copies, [node()]}, {type, set}, {attributes, record_info(fields, table)}]).
{atomic,ok}
4>
4> mnesia:dirty_write({table, a, 2}).
ok
5> mnesia:dirty_update_counter({table, a}, 1).
3
6>
6> mnesia:dirty_write({table, b, null}).
ok
7>
7> mnesia:dirty_update_counter({table, b}, 1).
1
```
If the `value`  is not an integer, the `Incr` is return.

## mnesia mnesia:transform_table
[Mnesia Cluster Table 在线增字段](https://tried.cc/erlang-001-mnesia-upgrade-md/)

## backup

``` erlang
mnesia:backup("path_name").
```
