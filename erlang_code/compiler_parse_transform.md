# compiler parse_transform option

## compiler module supoort parse_transform

``` erlang
{parse_transform,Module}
	Causes the parse transformation function Module:parse_transform/2 to be applied to the parsed code before the code is checked for errors.
```
The `Module` file must export the `parse_transform` function, with 2 arguments.

## the abstract_code
use the compiler option, debug_info and checkout the abstract code of a module

``` erlang
erlc +debug_info test.erl
{ok,{_,[{abstract_code,{_,AC}}]}} = beam_lib:chunks(filename:dirname(code:which(test)) ++ "/test.beam",[abstract_code]).
```
lager use the parse_transform option too.
