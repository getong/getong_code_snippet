#erlang re

## delete the beginning and the end white space

``` erlang
re:replace(String, "(^\\s+)|(\\s+$)", "", [global, {return, list}]).
```
