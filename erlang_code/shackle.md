# shackle
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
