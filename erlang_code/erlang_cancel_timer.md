#erlang cancel timer
cancel the timer and check the result.
copy from ejabberd mod_pind.erl

``` erlang
-spec cancel_timer(reference()) -> ok.
cancel_timer(TRef) ->
    case erlang:cancel_timer(TRef) of
      false ->
	  receive {timeout, TRef, _} -> ok after 0 -> ok end;
      _ -> ok
    end.
```
If the result is `false`, then delete the `timeout` msg from the mailbox.
