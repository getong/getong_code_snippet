# shackle

## shackle_compile
For speed reason, shackle encode two function into a module, and compile it into memory.
One is options/1, the other is server_name/2.

The module is like this:

```
-module(shackle_pool_utils).
-export([options/1, server_name/2]).

%% The pool here is hardcode to be string words, for example abc
options(Pool) ->
	OPtionList;
options(_) ->
	undefined.

%% The server_name return the poolName nth name
server_name(PoolName, Num)	 ->
	PoolName_Num;
server_name(_, _)	 ->
	undefined.
```

## shackle behaviours

``` erlang
handl_request function encode the data to be send out, just encode, the shackle will send it out.

handle_data decode the data from the socket, and shackle will send it to the receiver.
```

## shackle queue
shackle_queue use a ets public table to act as a queue.
ets:take/1 will get the element out and delete it from the ets table, but this function appear in erlang 18 or 19.

``` erlang
-ifdef(ETS_TAKE).

ets_take(Tid, Key) ->
    ets:take(Tid, Key).

-else.

ets_take(Tid, Key) ->
    case ets:lookup(Tid, Key) of
        [] ->
            [];
        Objects ->
            ets:delete(Tid, Key),
            Objects
    end.

-endif.
```
In the rebar.config

``` erlang
{platform_define, "18|19", 'ETS_TAKE'},
```
