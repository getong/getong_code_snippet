# centos daily operations

## set timezone

``` shell
# timedatectl list-timezones
# timedatectl set-timezone Asia/Shanghai
```

## install fastestmirror

``` shell
yum install yum-plugin-fastestmirror
```
Edit /etc/yum/pluginconf.d/fastestmirror.conf

``` shell
enabled=1
exclude=.gov, facebook
```
The fetch the new packages cache

``` shell
yum clean all
yum makecache

yum check-update
yum upgrade
```

## yumdownloader

``` shell
# yum install yum-utils
# yum check-update

```
Note that the package name and repos output here.

``` shell
# cd /var/cache/yum/x86_64/7/updates/packages ## the updates is the repo that download package
# yumdownloader package_names
```
