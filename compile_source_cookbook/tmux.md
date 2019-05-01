# compile tmux

## install the deps

CentOS

``` shell
yum install -y gcc libevent-devel ncurses-devel glibc-static automake
```

Debian

``` shell
apt-get  install -y gcc libevent-dev ncurses-dev build-essential automake
```

## compile and install
```shell
$ export VERSION=2.9a
$ wget -c https://github.com/tmux/tmux/releases/download/$VERSION/tmux-$VERSION.tar.gz
$ tar xzf tmux-$VERSION.tar.gz
$ cd tmux-$VERSION
$ ./autogen.sh
$ ./configure --prefix=/usr/local/tmux-$VERSION
$ make
$ sudo make install
```
