# erlang heart usage

## basic usage

``` shell
erl -heart -env HEART_BEAT_TIMEOUT 10 -env HEART_COMMAND boot_bsc
```

The meaning here is that, if the heart application not get the response in 10 seconds, then execute the boot_bsc script.

## Yaws use heart.
Get more example from the yaws.
