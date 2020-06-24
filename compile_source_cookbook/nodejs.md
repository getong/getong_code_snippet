# compile nodejs

``` shell
sudo apt-get build-dep nodejs
export VERSION=v14.4.0
wget -c https://nodejs.org/dist/$VERSION/node-$VERSION.tar.gz
tar xzf node-$VERSION.tar.gz
cd node-$VERSION
./configure --prefix=/usr/local/node-$VERSION
make -j`nproc`
sudo make install
```
