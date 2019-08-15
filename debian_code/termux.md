# termux

## download the apk file
[Termux](https://termux.com/)
[F-Droid Termux](https://f-droid.org/repository/browse/?fdid=com.termux)

## tsinghua mirror
see [Termux 镜像使用帮助](https://mirrors.tuna.tsinghua.edu.cn/help/termux/)

``` shell
sed -i 's@^\(deb.*stable main\)$@#\1\ndeb https://mirrors.tuna.tsinghua.edu.cn/termux stable main@' $PREFIX/etc/apt/sources.list
apt update && apt upgrade

# or change the $PREFIX/etc/apt/sources.list
# The termux repository mirror from TUNA:
deb https://mirrors.tuna.tsinghua.edu.cn/termux stable main

## then run
pkg up
```

## other resource
[Termux 高级终端安装使用配置教程](https://www.sqlsec.com/2018/05/termux.html)
