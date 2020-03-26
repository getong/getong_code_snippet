q# julia compile from source

```
export VERSION=1.4.0
wget -c
tar xzf julia-$VERSION-full.tar.gz
cd julia-$VERSION
sudo apt-get build-dep -y julia
echo "USE_BINARYBUILDER=0" >> Make.user
make
```
check the avx cpu option

``` shell
grep avx /proc/cpuinfo
```
