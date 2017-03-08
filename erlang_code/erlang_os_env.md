#erlang OS environment

## os:getenv/1 works in linux, windows probably not

```
%% in linux works, windows not
1> os:getenv("HOME").


```
## init:get_argument/1 works on most OS, but only a few arguments.

```
1>init:get_argument(home).
```
