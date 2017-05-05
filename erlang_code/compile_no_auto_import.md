# compile no auto import

``` erlang
-module(no_auto_import).

-compile({no_auto_import, [get/0, put/2]}).

-export([
		 start/0
		]).


start() ->
	A = get(),
	B = get(),

	put(A, B).

get() ->
	1.

put(A, B) ->
	io:format("A:~p, B:~p~n", [A, B]).

```

use the code:

``` shell
$ erlc no_auto_import.erl
$ erl
1>no_auto_import:start().
A:1, B:1
```
