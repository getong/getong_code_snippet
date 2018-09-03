# ets

## ets_lru
The entry info includes the key, value, atime, ctime. In order to keep all this info, there are three ets tables created, they are object_table, ct_table, at_table.
As the insert might update the old inserted data, the insert operation will update_element the old data.

``` erlang
handle_call({insert, Key, Val}, _From, St) ->
    NewATime = erlang:now(),
    Pattern = #entry{key=Key, atime='$1', _='_'},
    case ets:match(St#st.objects, Pattern) of
        [[ATime]] ->
            Update = {#entry.val, Val},
            true = ets:update_element(St#st.objects, Key, Update),
            true = ets:delete(St#st.atimes, ATime),
            true = ets:insert(St#st.atimes, {NewATime, Key});
        [] ->
            Entry = #entry{key=Key, val=Val, atime=NewATime, ctime=NewATime},
            true = ets:insert(St#st.objects, Entry),
            true = ets:insert(St#st.atimes, {NewATime, Key}),
            true = ets:insert(St#st.ctimes, {NewATime, Key})
    end,
    {reply, ok, St, 0};
```
