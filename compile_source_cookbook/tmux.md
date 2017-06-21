# compile tmux

## download the tmux
```shell
wget -c https://github.com/tmux/tmux/releases/download/2.5/tmux-2.5.tar.gz
```

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
$ tar xzf tmux-2.5.tar.gz
$ cd tmux-2.5
$ ./autogen.sh
$ ./configure --prefix=/usr/local/tmux-2.5
$ make
$ sudo make install
```
