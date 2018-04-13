# alpine

## apk
apk is alpine package manager

``` shell
apk add alpine-sdk
```
Very similar to apt-get in debian


## glibc
alpine use musl to replace glibc
``` shell
apk add musl-dev
```

## gcc, g++, git

``` shell
apk add ca-certificates openssl git gcc g++
```

## timezone

copy from [Setting the timezone](https://wiki.alpinelinux.org/wiki/Setting_the_timezone)
``` shell
apk add tzdata && cp -r -f /usr/share/zoneinfo/Asia/Shanghai /etc/localtime
apk del tzdata
```
