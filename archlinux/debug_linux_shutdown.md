# debug linux kernel shutdown

## Check which processes are causing long shutdown in Linux
>>>
When you shut down your Linux system, it sends the sigterm and politely asks the running processes to stop. Some processes misbehave and they ignore the sigterm and keep on running.
This could cause a delay to the shutdown process as your system will wait for the running processes to stop for a predefined time period. After this time period, it sends the kill signal to force stop all the remaining running processes and shuts down the system. I recommend reading about sigterm vs sigkill to understand the difference.

``` shell
journalctl -rb -1
```
copy from [Investigate and Fix Long Shutdown Time in Linux](https://itsfoss.com/long-shutdown-linux/)

##

``` shell
who -b

journalctl --list-boots

journalctl -b -1 -n
```

copy from [How to Find Linux Reboot Reason?](https://geekflare.com/check-linux-reboot-reason/)

## halt
You can do (sudo) `halt` instead of `poweroff`
copy from [Kernel 5.9.11 Shutdown](https://bbs.archlinux.org/viewtopic.php?id=261086)
