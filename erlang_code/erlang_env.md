## erlang env

## get local application env only with key, avoid application name every time
```
% copy  from riak_ensemble_config.erl
tick() ->
    get_env(ensemble_tick, 500).

get_env(Key, Default) ->
    application:get_env(riak_ensemble, Key, Default).
```

## epoch get_env function

``` erlang
%% This function is similar to application:get_env/2, except
%% 1. It uses the setup:get_env/2 function, which supports a number
%%    of useful variable expansions (see the setup documentation)
%% 2. It supports a hierarchical key format, [A,B,...], where each
%%    part of the key represents a level in a tree structure.
%% Example:
%% if get_env(A, a) returns {ok, {a, [{1, foo}, {2, bar}]}}, or
%% {ok, [{a, [{1, foo}, {2, bar}]}]}, then
%% get_env(A, [a,1]) will return {ok, foo}
-spec get_env(atom(), atom() | list()) -> undefined | {ok, any()}.
get_env(App, [H|T]) ->
    case setup:get_env(App, H) of
        {ok, V} ->
            get_env_l(T, V);
        undefined ->
            undefined
    end;
get_env(App, K) when is_atom(K) ->
    setup:get_env(App, K).

get_env(App, K, Default) ->
    case get_env(App, K) of
        {ok, V}   -> V;
        undefined -> Default
    end.

get_env_l([], V) ->
    {ok, V};
get_env_l([H|T], [_|_] = L) ->
    case lists:keyfind(H, 1, L) of
        {_, V} ->
            get_env_l(T, V);
        false ->
            undefined
    end;
get_env_l(_, _) ->
    undefined.
```

## erlang vm args

``` erlang
-smp auto +P 134217727 +K true +A 64 -rate 1200 +stbt db +scl false +sfwi 500 +spp true +zdbbl 8092
```
copy from [Improve erlang cowboy performance](https://stackoverflow.com/questions/53667669/improve-erlang-cowboy-performance)
