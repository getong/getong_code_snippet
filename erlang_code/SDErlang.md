#SDErlang
[SDErlang How To](http://www.dcs.gla.ac.uk/research/sd-erlang/howto.html)
[sd-install.sh](https://raw.githubusercontent.com/release-project/otp/17.4-rebased/sd-install.sh)
compile methods:

``` shell
git clone https://github.com/release-project/otp release-project-otp
cd release-project-otp
git checkout 19.3-rebased
git submodule init
git submodule update
export ERL_TOP=PWD
export PATH=ERL_TOP/bin:PATH
./otp_build autoconf
./configure --prefix=/usr/local/otp-19.3-rebased
make clean
make
make tests
export LANG=en
make docs
sudo make install
sudo make install-docs
```
