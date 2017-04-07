#erlang universaltime, localtime, posixtime

``` erlang
1> erlang:universaltime().
{{2017,4,7},{10,4,38}}
2> erlang:universaltime_to_posixtime({{2017,4,7},{10,4,38}}).
1491559478
3> erlang:universaltime_to_localtime({{2017,4,7},{10,4,38}}).
{{2017,4,7},{18,4,38}}
4>erlang:posixtime_to_universaltime(1491559478).
{{2017,4,7},{10,4,38}}

```
