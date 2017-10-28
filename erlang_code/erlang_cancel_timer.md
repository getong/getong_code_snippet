# erlang cancel timer
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

## use erlang:cancel_timer(Ref, [{async, true}, {info, false}])
``` shell
Options:

{async, Async}
Asynchronous request for cancellation. Async defaults to false, which causes the cancellation to be performed synchronously. When Async is set to true, the cancel operation is performed asynchronously. That is, cancel_timer() sends an asynchronous request for cancellation to the timer service that manages the timer, and then returns ok.

{info, Info}
Requests information about the Result of the cancellation. Info defaults to true, which means the Result is given. When Info is set to false, no information about the result of the cancellation is given.

When Async is false: if Info is true, the Result is returned by erlang:cancel_timer(). otherwise ok is returned.

When Async is true: if Info is true, a message on the form {cancel_timer, TimerRef, Result} is sent to the caller of erlang:cancel_timer() when the cancellation operation has been performed, otherwise no message is sent.


```
