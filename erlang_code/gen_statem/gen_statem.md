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

## repeat_state and repeat_state_and_data

>> The gen_statem keeps the current state, or does a state transition to the current state if you like, sets NewData, and executes all Actions. If the gen_statem runs with state enter calls, the state enter call is repeated, see type transition_option(), otherwise repeat_state is the same as keep_state.

## gen_statem:call and timeout option
The gen_statem:call/3 function use clean_timeout as default, it will spawn a proxy process, and so in the gen_statem callback module, the `Caller` does not contains the original caller process, the `Caller` contains the proxy process instead. So if using the default timeout or the clean_timeout, take the original caller process pid into the `call` function, and deal with `Caller` variable carefully.
The timeout argument, for example, like 5000, is spawn in a new process. And then gen_statm:call/3 will not pass the `Caller` as the call process. Use dirty_timeout.
But in the return data, the timeout, like 5000, will generate a `timeout` msg. Just like other behaviours.

## three types of timeout
```
 event_timeout, generic_timeout, state_timeout
Any event cancels an event_timeout() so a zero time event time-out is only generated if the event queue is empty.

A state change cancels a state_timeout() and any new transition option of this type belongs to the new state.
```


## next_event
```
enter_action() =
    hibernate |
    {hibernate, Hibernate :: hibernate()} |
    timeout_action() |
    reply_action()

action() =
    postpone |
    {postpone, Postpone :: postpone()} |
    {next_event, EventType :: event_type(), EventContent :: term()} |
    enter_action()

state_callback_result(ActionType) =
    {keep_state, NewData :: data()} |
    {keep_state, NewData :: data(), Actions :: [ActionType] | ActionType} |
    keep_state_and_data |
    {keep_state_and_data, Actions :: [ActionType] | ActionType} |
    {repeat_state, NewData :: data()} |
    {repeat_state, NewData :: data(), Actions :: [ActionType] | ActionType} |
    repeat_state_and_data |
    {repeat_state_and_data, Actions :: [ActionType] | ActionType} |
    stop |
    {stop, Reason :: term()} |
    {stop, Reason :: term(), NewData :: data()} |
    {stop_and_reply, Reason :: term(), Replies :: [reply_action()] | reply_action()} |
    {stop_and_reply, Reason :: term(), Replies :: [reply_action()] | reply_action(), NewData :: data()}

ActionType is enter_action() if the state callback was called with a state enter call
and action() if the state callback was called with an event.
```
see [gen_fsm から gen_statem へ](https://blog.jxck.io/entries/2017-05-18/gen_statem.html) for a example, google translate might be help.
