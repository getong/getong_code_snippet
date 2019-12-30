# ruby
Compile ruby

``` shell
wget -c https://cache.ruby-lang.org/pub/ruby/2.7/ruby-2.7.0.tar.gz
tar xaf ruby-2.7.0.tar.xz
cd ruby-2.7.0
./configure --prefix=/usr/local/ruby-2.7.0
make
make capi
make check
sudo make install
```

See the reference: [Ruby-2.6.5](http://www.linuxfromscratch.org/blfs/view/cvs/general/ruby.html)

Besides, compile on Debian, See the [Ruby – Installation and compilation from sources on Debian](https://mensfeld.pl/2014/10/ruby-installation-and-compilation-from-sources-on-debian/)
