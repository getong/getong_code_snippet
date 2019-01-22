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
It often add gcc and g++ in the production environment.

## timezone

copy from [Setting the timezone](https://wiki.alpinelinux.org/wiki/Setting_the_timezone)
``` shell
apk add tzdata && cp -r -f /usr/share/zoneinfo/Asia/Shanghai /etc/localtime
apk del tzdata
```

## use the ustc mirror

``` shell
sed -i 's/dl-cdn.alpinelinux.org/mirrors.ustc.edu.cn/g' /etc/apk/repositories
```
