#compile erlang

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
wget -c https://github.com/erlang/otp/archive/OTP-19.3.2.tar.gz
tar xzf OTP-19.3.2.tar.gz
cd otp-OTP-19.3.2
export ERL_TOP=$PWD
export PATH=$ERL_TOP/bin:$PATH

./otp_build autoconf

./configure --prefix=/usr/local/otp_src_19.3.2
make clean
#using 4 cores cpu
make -j4 && make -j4 tests

export LANG=en
make -j4 docs

sudo make install && sudo make install-docs
```
