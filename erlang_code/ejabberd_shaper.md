# ejabberd shaper

## load_from_config

``` erlang
load_from_config() ->
    Shapers = ejabberd_config:get_option(
                shaper, fun(V) -> V end, []),
    case mnesia:transaction(
           fun() ->
                   lists:foreach(
                     fun({Name, MaxRate}) ->
                             mnesia:write(#shaper{name = {Name, global},
                                                  maxrate = MaxRate})
                     end, Shapers)
           end) of
        {atomic, ok} ->
            ok;
        Err ->
            {error, Err}
    end.
```
It get the config info and store the info into mnesia.

## get_max_rate and new

``` erlang
get_max_rate(none) ->
    none;
get_max_rate(Name) ->
    case ets:lookup(shaper, {Name, global}) of
	[#shaper{maxrate = R}] ->
	    R;
	[] ->
	    none
    end.

new(none) ->
    none;
new(Name) ->
    MaxRate = case ets:lookup(shaper, {Name, global}) of
                  [#shaper{maxrate = R}] ->
                      R;
                  [] ->
                      none
              end,
    new1(MaxRate).

new1(none) -> none;
new1(MaxRate) ->
    #maxrate{maxrate = MaxRate, lastrate = 0.0,
	     lasttime = p1_time_compat:system_time(micro_seconds)}.
```
For speed reason, it use `ets:lookup` function to get the info from the `mnesia`.

## update

``` abap
update(none, _Size) -> {none, 0};
update(#maxrate{} = State, Size) ->
    MinInterv = 1000 * Size /
		  (2 * State#maxrate.maxrate - State#maxrate.lastrate),
    Interv = (p1_time_compat:system_time(micro_seconds) - State#maxrate.lasttime) /
	       1000,
    ?DEBUG("State: ~p, Size=~p~nM=~p, I=~p~n",
	   [State, Size, MinInterv, Interv]),
    Pause = if MinInterv > Interv ->
		   1 + trunc(MinInterv - Interv);
	       true -> 0
	    end,
    NextNow = p1_time_compat:system_time(micro_seconds) + Pause * 1000,
    Div = case NextNow - State#maxrate.lasttime of
        0 -> 1;
        V -> V
    end,
    {State#maxrate{lastrate =
		       (State#maxrate.lastrate +
			  1000000 * Size / Div)
			 / 2,
		   lasttime = NextNow},
     Pause}.
```
The `update` function return two elemnts tuple. The first is the new `shaper` state. The Second is the `Pause` value. The `Pause` value is a millisecond value, used to stop the process in `Pause` milliseconds. And the `Pause` value is often used with `erlang:send_after` function.

## traffic ratio control
`none` means not set traffic control. `max_rate` means traffic control sets maximum.
