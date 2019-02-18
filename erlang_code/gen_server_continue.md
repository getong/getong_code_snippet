# gen_server

## continue
erlang 21 introduces the `continue` feature, some code examples like below:
``` erlang
init(_) ->
    {ok, #state{}, {continue, ContinueInfo}}.

handle_continue(ContinueInfo, State) ->
    {noreply, State}.
```
Note that, the `init/1`, `handle_call/3`, `handle_cast/2`, `handle_info/2`, and even the `handle_continue/2` might return the `{continue, Continue}` argument.
As the doc says:

```
This callback is optional, so callback modules need to export it only if they return {continue,Continue} from another callback.
If continue is used and the callback is not implemented, the process will exit with undef error.
This function is called by a gen_server process whenever a previous callback returns {continue, Continue}.
handle_continue/2 is invoked immediately after the previous callback,
which makes it useful for performing work after initialization or for splitting the work in a callback in multiple steps,
updating the process state along the way.
```
