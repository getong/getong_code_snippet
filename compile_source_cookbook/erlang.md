# compile erlang

## kerl
[kerl](https://github.com/kerl/kerl)

``` shell
export KERL_BUILD_BACKEND=git
export OTP_GITHUB_URL="https://github.com/erlang/otp"
kerl update releases
kerl list releases
KERL_BUILD_DOCS=yes kerl build 19.3.2 19.3.2
kerl install 19.3.2 ~/kerl/19.3.2
source ~/kerl/19.3.2/activate
```

## compile from source

``` shell
wget -c https://github.com/erlang/otp/archive/OTP-19.3.3.tar.gz
tar xzf OTP-19.3.3.tar.gz
cd otp-OTP-19.3.3
export ERL_TOP=$PWD
export PATH=$ERL_TOP/bin:$PATH

./otp_build autoconf

./configure --prefix=/usr/local/otp_src_19.3.3
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
debian
``` shell
aptitude install autoconf libncurses-dev build-essential \
    libssl-dev libwxgtk3.0-dev libgl1-mesa-dev \
    libglu1-mesa-dev libpng3 default-jdk g++ libxml2-utils

apt-get build-dep erlang
```

centos

``` shell
yum groupinstall -y "Development Tools"
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
