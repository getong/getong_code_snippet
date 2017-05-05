# erlang array
array is defined as record

```
-record(array, {size :: non_neg_integer(),  %% number of defined entries
        max  :: non_neg_integer(),  %% maximum number of entries
                        %% in current tree
        default,    %% the default value (usually 'undefined')
                elements :: elements(_)         %% the tuple tree
           }).
```
the `size` is the size of the array.
fixed number is to set `max` to 0, otherwise it is not limit.

The `default` is used to add a default value to the tuple.

The `elements` is the tuple elements of the array.

The `array` module use tuple to store the element of a array.
