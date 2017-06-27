# ruby
Compile ruby

``` shell
wget -c http://cache.ruby-lang.org/pub/ruby/2.4/ruby-2.4.1.tar.xz
tar xaf ruby-2.4.1.tar.xz
cd ruby-2.4.1
./configure --prefix=/usr/local/ruby-2.4.1
make
make capi
sudo make install
```

See the reference: [Ruby-2.4.1](http://www.linuxfromscratch.org/blfs/view/cvs/general/ruby.html)
