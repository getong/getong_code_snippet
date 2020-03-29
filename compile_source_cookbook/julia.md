# julia compile from source

```
sudo apt-get build-dep -y julia
sudo apt-get install -y gfortran
export VERSION=1.4.0
wget -c https://github.com/JuliaLang/julia/releases/download/v$VERSION/julia-$VERSION-full.tar.gz
tar xzf julia-$VERSION-full.tar.gz
cd julia-$VERSION
echo "USE_BINARYBUILDER=0\nprefix=/usr/local/julia-$VERSION" >> Make.user
make -j `nproc`
make -j `nproc` docs
sudo make `nproc` install
```
check the avx cpu option

``` shell
grep avx /proc/cpuinfo
```

## change the llvm
change the line
```
JL_PRIVATE_LIBS-$(USE_SYSTEM_LLVM) += libLLVM libLLVM-8
```
to be

```
JL_PRIVATE_LIBS-$(USE_SYSTEM_LLVM) += libLLVM $(basename $(shell $(LLVM_CONFIG_HOST) --libnames))
```
copy from [make sure we install the suffixed LLVM library correctly](https://github.com/JuliaLang/julia/pull/35063/files)
