# compile erlang

## kerl
[kerl](https://github.com/kerl/kerl)

``` shell
export KERL_BUILD_BACKEND=git
export OTP_GITHUB_URL=“https://github.com/erlang/otp”
kerl update releases
kerl list releases
KERL_BUILD_DOCS=yes kerl build 19.3.2 19.3.2
kerl install 19.3.2 ~/kerl/19.3.2
source ~/kerl/19.3.2/activate
```

## compile from source

``` shell
export VERSION=22.2.1
wget -c https://github.com/erlang/otp/archive/OTP-$VERSION.tar.gz
tar xzf OTP-$VERSION.tar.gz
cd otp-OTP-$VERSION
export ERL_TOP=$PWD
export PATH=$ERL_TOP/bin:$PATH

./otp_build autoconf

./configure --prefix=/usr/local/otp_src_$VERSION
make clean
#using all cores of a cpu
make -j`nproc` && make -j`nproc` tests

export LANG=en
make -j`nproc` docs

sudo make install && sudo make install-docs
```

## g++: internal compiler error: Killed (program cc1plus)
The main reason is the memory is not enough, use the swap file to solve it.

``` shell
sudo dd if=/dev/zero of=/swapfile bs=64M count=16
sudo mkswap /swapfile
sudo swapon /swapfile
```

Then compile the erlang source, and after compiled it, stop the swap file.

``` shell
sudo swapoff /swapfile
sudo rm /swapfile
```


## install the deps
debian jessie
``` shell
aptitude install autoconf libncurses-dev build-essential \
    libssl-dev libwxgtk3.0-dev libgl1-mesa-dev \
    libglu1-mesa-dev libpng3 default-jdk g++ libxml2-utils

apt-get build-dep erlang
```
debian stretch
```shell
sudo apt-get build-dep -y erlang
sudo apt-get install -y autoconf libncurses5-dev build-essential \
    libssl-dev libwxgtk3.0-dev libgl1-mesa-dev \
    libglu1-mesa-dev libpng12-dev default-jdk g++ libxml2-utils
```
debian buster
install libgail-common libcanberra-gtk-module for erlang observer module
``` shell
sudo apt install -y libgail-common libcanberra-gtk-module
```

centos

``` shell
yum groupinstall -y “Development Tools”
yum install m4 openssl openssl-devel  unixODBC unixODBC-devel \
	make gcc gcc-c++ kernel-devel ncurses-devel libxslt \
	fop java-1.8.0-openjdk-devel wxGTK-gl wxGTK-devel tk
```

## erlang-rpm project
There is a project for to package erlang install rpm in centos, it is made by RabbitMQ.
See [erlang-rpm](https://github.com/rabbitmq/erlang-rpm)
With some patches in its script.

Something releated:
[Use ESL erlang deb, provide fake erlang-nox package so deps behave](https://gist.github.com/RJ/2284940)
[How to install RabbitMQ with the latest Erlang release on Debian](https://blog.eriksen.com.br/en/how-install-rabbitmq-latest-erlang-release-debian)


## centos 7 compile erlang

Enable EPEL repo in CentOS 7
``` shell
$ sudo yum install epel-release
```
See [Install EPEL, IUS, and Remi repositories on CentOS and Red Hat](https://support.rackspace.com/how-to/install-epel-and-additional-repositories-on-centos-and-red-hat/)

install the wxdiget from elpa

``` shell
$ sudo yum install wxGTK-gl wxGTK-devel
```
> wx* packages have never been part of CentOS base/updates. They are in EPEL.
See [wxBase/wxGTK/wxGTK-gl packages in centos 7 repos?](https://www.centos.org/forums/viewtopic.php?t=50620)



## erlang centos china mirror
See [使用国内源安装erlang](http://www.jianshu.com/p/27197d58e94c)

``` shell
# wget https://packages.erlang-solutions.com/erlang-solutions-2.0-1.noarch.rpm
# rpm -Uvh erlang-solutions-2.0-1.noarch.rpm
```
Edit /etc/yum.repos.d/erlang_solutions.repo
centos 6

``` shell
baseurl=https://mirrors.tuna.tsinghua.edu.cn/erlang-solutions/centos/6/
```

centos 7
``` shell
baseurl=https://mirrors.tuna.tsinghua.edu.cn/erlang-solutions/centos/7/
```

## compile erlang from source using self compiled openssl
see [OpenSSL error building Erlang OTP from source](https://stackoverflow.com/questions/6618233/openssl-error-building-erlang-otp-from-source)

compile openssl
``` shell
$ tar xzf openssl-1.0.2l.tar.gz
$ cd openssl-1.0.2l
$ sh config shared -fPIC --prefix=/usr/local/openssl-1.0.2l
$ make
$ make test
$ sudo make install
```

compile erlang using compiled openssl

``` shell
./configure --with-ssl=/usr/local/openssl-1.0.2l
```
