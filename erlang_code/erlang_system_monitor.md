# erlang system monitor
copy from alarms_server.erl

```
init([]) ->
    erlang:system_monitor(
      self(), [{long_gc, alarms_utils:get_cfg(long_gc)},
               {large_heap, alarms_utils:get_cfg(large_heap)},
               busy_port,
               busy_dist_port]),
    {ok, _} = mnesia:subscribe(system),
    ok = net_kernel:monitor_nodes(true, [{node_type, all}, nodedown_reason]),
    {ok, #state{}}.

handle_info(Info, State) ->
    case msg_to_alarm(Info) of
        false ->
            ok;
        {AlarmType, Details} ->
            set_alarm(AlarmType, Details)
    end,
    {noreply, State}.


%% from erlang:system_monitor/2
msg_to_alarm({monitor, GcPid, long_gc, Info}) ->
    {long_gc, {GcPid, Info}};
msg_to_alarm({monitor, GcPid, large_heap, Info}) ->
    {large_heap, {GcPid, Info}};
msg_to_alarm({monitor, SusPid, busy_port, Port}) ->
    {busy_port, {SusPid, Port}};
msg_to_alarm({monitor, SusPid, busy_dist_port, Port}) ->
    {busy_dist_port, {SusPid, Port}};

%% from mnesia:subscribe/1
msg_to_alarm({mnesia_overload, Details}) ->
    {mnesia_overload, Details};
msg_to_alarm({inconsistent_database, Context, Node}) ->
    {inconsistent_database, {Context, Node}};
msg_to_alarm({mnesia_fatal, Format, Args, _BinaryCore}) ->
    {mnesia_fatal, {Format, Args}};
msg_to_alarm({mnesia_error, Format, Args}) ->
    {mnesia_error, {Format, Args}};
msg_to_alarm({mnesia_up, Node}) ->
    {mnesia_up, Node};
msg_to_alarm({mnesia_down, Node}) ->
    {mnesia_down, Node};

%% from net_kernel:monitor_nodes/2
msg_to_alarm({nodeup, Node, InfoList}) ->
    {nodeup, {Node, InfoList}};
msg_to_alarm({nodedown, Node, InfoList}) ->
    {nodedown, {Node, InfoList}};

msg_to_alarm(_) ->
    false.

set_alarm(AlarmType, Info) ->
    alarm_handler:set_alarm({AlarmType, Info}).
```
