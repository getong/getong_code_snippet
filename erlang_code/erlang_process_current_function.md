# erlang process current function
To know what the process current function running:

``` erlang
erl>erlang:process_info(Pid, current_function).
{current_function,{httpc,handle_answer,3}}

... Time passed by

erl>erlang:process_info(Pid, current_function).
{current_function,{gen_server,loop,6}}

```
Through this method, I found out what caused our system key process timeout.
