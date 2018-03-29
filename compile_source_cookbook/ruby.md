# ruby
Compile ruby

``` shell
wget -c http://cache.ruby-lang.org/pub/ruby/2.5/ruby-2.4.1.tar.xz
tar xaf ruby-2.5.1.tar.xz
cd ruby-2.5.1
./configure --prefix=/usr/local/ruby-2.5.1
make
make test
make capi
sudo make install
```

See the reference: [Ruby-2.5.0](http://www.linuxfromscratch.org/blfs/view/cvs/general/ruby.html)

Besides, compile on Debian, See the [Ruby – Installation and compilation from sources on Debian](https://mensfeld.pl/2014/10/ruby-installation-and-compilation-from-sources-on-debian/)
