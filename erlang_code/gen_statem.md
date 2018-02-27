# gen_statem

## gen_statem is used to replace gen_fsm
The `gen_fsm` is complex and hanrd to learn, the `gen_statem` is the new state manchine behaviour.
The `gen_fsm` is deprecated in 20, and its doc is deteled in 20.

## callback_mode
`state_functions` has many state_name functions.
`handle_event_function` only has `handle_event/4` function.

## state_enter callback_mode
copy from [Erlang/OTP设计原则（文档翻译)](https://www.cnblogs.com/-wyp/p/6892632.html)
>> 在 callback_mode/0 函数的返回列表中加入 state_enter，会在每次状态改变的时候传入参数 (enter, OldState, ...) 调用一次回调函数。你只需像事件一样处理这些请求即可：

``` erlang
callback_mode() ->
    [state_functions,state_enter].

...

locked(enter, _OldState, Data) ->
    do_lock(),
    {keep_state,Data#{remaining => Code}};
...

open(enter, _OldState, _Data) ->
    do_unlock(),
    {keep_state_and_data, [{state_timeout,10000,lock}]};
```
