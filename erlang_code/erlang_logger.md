# logger
The `logger` module is introduced since erlang 21, and it has the same ability with lager.

## Only print the error msg

``` erlang
$erl
1> logger:log(notice, "test msg").
=NOTICE REPORT==== 8-Sep-2018::23:08:58.969447 ===
test msg
ok
2> NewFormatter = {logger_formatter, #{template => [msg, "\n"]}}.
{logger_formatter,#{template => [msg,"\n"]}}
3> logger:set_handler_config(default, formatter, NewFormatter).
ok
4> logger:log(notice, "test msg").
test msg
ok
```
or add the kernel option in the sys.config

``` erlang
{kernel,
    [{logger,
        [{handler,default,logger_std_h,
            #{formatter => {logger_formatter,
                #{template => [msg,"\n"]}}}}
    ]}]}
```
