# erlang time operation

## 时间操作，例如前一天、后一天，最好的方案是先把时间换算成unix时间戳， 然后进行计算，最后换算回来日期。

## ejabberd 对erlang不同的版本的时间函数进行了封装，确保了使用正确的函数
[p1_utils](https://github.com/processone/p1_utils)
p1_time_compat.erl 这个模块可以找到对应版本的时间方法.

## statistics of time

``` erlang
{T1, _} = statistics(wall_clock),
{T2, _} = statistics(wall_clock),
?INFO_MSG("ejabberd ~s is started in the node ~p in ~.2fs",
	[?VERSION, node(), (T2-T1)/1000]),

```
copy from ejabberd_app.erl


## uptime
Prints the node uptime (as specified by erlang:statistics(wall_clock)) in human-readable form.

``` erlang
%%
%% uptime/0
%%

-spec uptime() -> 'ok'.

uptime() ->
    io:format("~s~n", [uptime(get_uptime())]).

uptime({D, {H, M, S}}) ->
    lists:flatten(
      [[ io_lib:format("~p days, ", [D]) || D > 0 ],
       [ io_lib:format("~p hours, ", [H]) || D+H > 0 ],
       [ io_lib:format("~p minutes and ", [M]) || D+H+M > 0 ],
       io_lib:format("~p seconds", [S])]).

get_uptime() ->
    {UpTime, _} = erlang:statistics(wall_clock),
    calendar:seconds_to_daystime(UpTime div 1000).
```
copy from c.erl
