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

## stop supervisor report

``` erlang
logger:add_primary_filter(stop_sup_reports,{fun(#{log:=#{label:={supervisor,child_terminated}}},_)
-> stop; (_,_) -> ignore end, ok}).

logger:add_primary_filter(stop_sup_reports,{fun(#{msg:={report,#{label:={supervisor,child_terminated},report:=R}}},_)
-> case proplists:get_value(supervisor,R) of {local,fah_sup} -> stop; _ ->
ignore end; (_,_) -> ignore end, ok}).


logger:add_primary_filter(stop_sup_reports,{fun(#{msg:={report,#{label:={supervisor,child_terminated},report:=R}}},_)
-> Child = proplists:get_value(offender,R), case
proplists:get_value(id,Child) of fah -> stop; _ -> ignore end; (_,_) ->
ignore end, ok}).
```
copy from [Prevent my crashing gen_server to produce any error message](http://erlang.org/pipermail/erlang-questions/2018-September/096339.html)
