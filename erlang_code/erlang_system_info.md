# erlang system_info

## process_limit

> Returns the maximum number of simultaneously existing processes at the local node.
> The value is given as an integer.
> This limit can be configured at startup by using command-line flag +P in erl(1).

``` erlang
1 > erlang:system_info(process_limit).
262144

```
