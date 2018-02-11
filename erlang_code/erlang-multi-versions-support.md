Erlang/OTP 在各个版本中引入了很多新特性，导致某些函数在不同的版本中不可用，因此编写版本兼容的代码很有必要。

# 1. 使用indef或者ifndef条件编译

```
%% 在riak_core
%% 在rebar.config 添加
{erl_opts, [
            {platform_define, "^R15", "old_hash"}
            ]
    }.

%% 在代码里面使用
-ifndef(old_hash).
sha(Bin) ->
    crypto:hash(sha, Bin).
-else.
sha(Bin) ->
    crypto:sha(Bin).
-endif.
```
推荐使用这种方式。像rand模块、不同版本的spec标记、crypto的 strong_rand_bytes/1函数等都可以使用这种方法

# 2. 使用try...catch...
```
%% 忘记在哪个项目使用了, 仿照下面的例子改写一个
ts() ->
    try
        %% 19.2
        erlang:system_time(microsecond)
    catch
        _:_ ->
            {MegaSeconds, Seconds, MicroSeconds} = erlang:now(),
            (MegaSeconds * 1000000 + Seconds) * 1000000 + MicroSeconds
    end.

```

# 3.使用erlang:function_exported/3函数检测是否有新的函数
```
%% emqttd/src/emqttd_guid.erl
ts() ->
    case erlang:function_exported(erlang, system_time, 1) of
        true -> %% R18
            erlang:system_time(micro_seconds);
        false ->
            {MegaSeconds, Seconds, MicroSeconds} = os:timestamp(),
            (MegaSeconds * 1000000 + Seconds) * 1000000 + MicroSeconds
    end.
```

# 4.鸵鸟埋头大法, 屏蔽编译错误
```
%% 在ejabberd
%% rebar.config
{erl_opts, [nowarn_deprecated_function]}.
```
不推荐使用。有些废弃的函数在模块中没有被删除掉，但不排除以后会。


## unsupported api
``` erlang
erlang:now() 
random module
gen_fsm module
erlang:get_stacktrace() will be changed in the future
```
