# get local application env only with key, avoid application name every time
```
% copy  from riak_ensemble_config.erl
tick() ->
    get_env(ensemble_tick, 500).

get_env(Key, Default) ->
    application:get_env(riak_ensemble, Key, Default).
```
