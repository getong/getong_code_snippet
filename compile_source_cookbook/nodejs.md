# compile nodejs

``` shell
sudo apt-get build-dep nodejs
export VERSION=v13.12.0
wget -c https://nodejs.org/dist/v13.11.0/node-$VERSION.tar.gz
tar xzf node-$VERSION.tar.gz
cd node-$VERSION
./configure --prefix=/usr/local/node-$VERSION
make -j`nproc`
sudo make install
```
