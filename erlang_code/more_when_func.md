use when in case of many nest code
```
%% in riak_ensemble riak_ensemble_lease.erl
check_lease({_, T}) ->
    case ets:lookup_element(T, lease, 2) of
        undefined ->
            false;
        Until ->
            case riak_ensemble_clock:monotonic_time_ms() of
                {ok, Time} when Time < Until ->
                    true;
                _ ->
                    false
            end
    end.
```
the `when` is used to avoid many nest codes.
