#erlang array
array is defined as record

```
-record(array, {size :: non_neg_integer(),  %% number of defined entries
        max  :: non_neg_integer(),  %% maximum number of entries
                        %% in current tree
        default,    %% the default value (usually 'undefined')
                elements :: elements(_)         %% the tuple tree
           }).
```
fixed number is to set `max` to 0, otherwise it is not limit.
