# erlang universaltime, localtime, posixtime

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
When storing files into the file system, it gets the localtime, and change it to the universaltime, but convert it into posix time as a number.
When getting the file atime, mtime, ctime, it fetch the posix time as a number, and then convert it into universaltime, and finally change it to localtime.

## loop cycle time need to check the begin and end time, in some situation, the time is the end, not the begin.
