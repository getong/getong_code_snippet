# archlinux install python2

## install python2

```
sudo pacman -S python2 python2-setuptools
```

## install ply, xlwt, xlrd

``` shell
wget -c https://files.pythonhosted.org/packages/e5/69/882ee5c9d017149285cab114ebeab373308ef0f874fcdac9beb90e0ac4da/ply-3.11.tar.gz \
    https://files.pythonhosted.org/packages/06/97/56a6f56ce44578a69343449aa5a0d98eefe04085d69da539f3034e2cd5c1/xlwt-1.3.0.tar.gz \
    https://files.pythonhosted.org/packages/a6/b3/19a2540d21dea5f908304375bd43f5ed7a4c28a370dc9122c565423e6b44/xlrd-2.0.1.tar.gz

tar xzf ply-3.11.tar.gz
cd ply-3.11
sudo python2 setup.py install

tar xzf xlwt-1.3.0.tar.gz
cd xlwt-1.3.0
sudo python2 setup.py install


tar xzf xlrd-2.0.1.tar.gz
cd xlrd-2.0.1
sudo python2 setup.py install

```
