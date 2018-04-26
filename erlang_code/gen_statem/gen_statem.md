# gen_statem

## Two callback modes
> The callback mode is selected when starting the gen_statem and after code change
> using the return value from Module:callback_mode/0.
>
> state_functions
> The state must be of type state_name() and one callback function per state,
> that is, Module:StateName/3, is used.
>
> handle_event_function
> The state can be any term and the callback function Module:handle_event/4 is used for all states.

## state_enter

```
callback_mode_result() =
    callback_mode() | [callback_mode() | state_enter()]


state_enter_result(State) =
    {next_state, State, NewData :: data()} |
    {next_state,
     State,
     NewData :: data(),
     Actions :: [enter_action()] | enter_action()} |
    state_callback_result(enter_action())
State is the current state and it can not be changed since the state callback was called with a state enter call.
```
