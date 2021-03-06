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

## install the zsh, termux-ohmyzsh
pkg install vim curl wget git unzip unrar zsh
sh -c "$(curl -fsSL https://github.com/Cabbagec/termux-ohmyzsh/raw/master/install.sh)"
pkg add nodejs yarn rust erlang
```

## other resource
[Termux 高级终端安装使用配置教程](https://www.sqlsec.com/2018/05/termux.html)

## install the python2, make, clang

``` shell
pkg add python2 make clang
```

## isntall the unstable repository

``` shell
pkg install unstable-repo
apt-get update
apt-get upgrade
pkg add elixir -y
```
