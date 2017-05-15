# ejabberd

compile ejabberd in debian

``` shell
sudo apt-get install libsqlite3-dev libyaml-dev libpam0g-dev
git clone https://github.com/processone/ejabberd
cd ejabberd
./autogen.sh
./configure --enable-all
make

```
