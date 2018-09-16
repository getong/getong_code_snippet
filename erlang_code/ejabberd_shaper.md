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

``` erlang
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

## the shaper in lager

``` erlang
-spec init(any()) -> {ok, #state{}}.
init([HighWaterMark, GlStrategy]) ->
    Flush = application:get_env(lager, error_logger_flush_queue, true),
    FlushThr = application:get_env(lager, error_logger_flush_threshold, 0),
    Shaper = #lager_shaper{hwm=HighWaterMark, flush_queue = Flush, flush_threshold = FlushThr, filter=shaper_fun(), id=?MODULE},
    Raw = application:get_env(lager, error_logger_format_raw, false),
    Sink = configured_sink(),
    {ok, #state{sink=Sink, shaper=Shaper, groupleader_strategy=GlStrategy, raw=Raw}}.

handle_event(Event, #state{sink=Sink, shaper=Shaper} = State) ->
    case lager_util:check_hwm(Shaper, Event) of
        {true, 0, NewShaper} ->
            eval_gl(Event, State#state{shaper=NewShaper});
        {true, Drop, #lager_shaper{hwm=Hwm} = NewShaper} when Drop > 0 ->
            ?LOGFMT(Sink, warning, self(),
                "lager_error_logger_h dropped ~p messages in the last second that exceeded the limit of ~p messages/sec",
                [Drop, Hwm]),
            eval_gl(Event, State#state{shaper=NewShaper});
        {false, _, #lager_shaper{dropped=D} = NewShaper} ->
            {ok, State#state{shaper=NewShaper#lager_shaper{dropped=D+1}}}
    end.
```
copy from error_logger_lager_h.erl

``` erlang
%% conditionally check the HWM if the event would not have been filtered
check_hwm(Shaper = #lager_shaper{filter = Filter}, Event) ->
    case Filter(Event) of
        true ->
            {true, 0, Shaper};
        false ->
            check_hwm(Shaper)
    end.

%% Log rate limit, i.e. high water mark for incoming messages

check_hwm(Shaper = #lager_shaper{hwm = undefined}) ->
    {true, 0, Shaper};
check_hwm(Shaper = #lager_shaper{mps = Mps, hwm = Hwm}) when Mps < Hwm ->
    %% haven't hit high water mark yet, just log it
    {true, 0, Shaper#lager_shaper{mps=Mps+1, lasttime = os:timestamp()}};
check_hwm(Shaper = #lager_shaper{lasttime = Last, dropped = Drop}) ->
    %% are we still in the same second?
    {M, S, _} = Now = os:timestamp(),
    case Last of
        {M, S, N} ->
            %% still in same second, but have exceeded the high water mark
            NewDrops = case should_flush(Shaper) of
                           true ->
                               discard_messages(Now, Shaper#lager_shaper.filter, 0);
                           false ->
                               0
                       end,
            Timer = case erlang:read_timer(Shaper#lager_shaper.timer) of
                        false ->
                            erlang:send_after(trunc((1000000 - N)/1000), self(), {shaper_expired, Shaper#lager_shaper.id});
                        _ ->
                            Shaper#lager_shaper.timer
                    end,
            {false, 0, Shaper#lager_shaper{dropped=Drop+NewDrops, timer=Timer}};
        _ ->
            erlang:cancel_timer(Shaper#lager_shaper.timer),
            %% different second, reset all counters and allow it
            {true, Drop, Shaper#lager_shaper{dropped = 0, mps=0, lasttime = Now}}
    end.

should_flush(#lager_shaper{flush_queue = true, flush_threshold = 0}) ->
    true;
should_flush(#lager_shaper{flush_queue = true, flush_threshold = T}) ->
    {_, L} = process_info(self(), message_queue_len),
    L > T;
should_flush(_) ->
    false.
```
copy from lager_util.erl

The code style is much the same with `ejabberd_shaper.erl`. Make the config data in the data state, and check some value with it.
