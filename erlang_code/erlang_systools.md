# erlang systools

## erlang version below 19(includes 19), app file do not add {mod, []} option
The erlang docs says, the {mod, []} is default option, but the systools_make.erl check_item/2 function does not handle this.

``` erlang
check_item({_,{mod,{M,A}}},_) when is_atom(M) ->
    {M,A};
check_item({_,{mod,[]}},_) -> % default mod is [], so accept as entry
    [];
...
```

20 fix this bug.
