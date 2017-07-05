# vagrant plugin install
vagrant plugin install is very difficult, just to download the gems from the [Ruby China Gems](https://gems.ruby-china.org)
See the [解决中国局域网中 vagrant plugin install vagrant-vbguest 出错的问题](https://lotreal.github.io/2015/12/08/install-vagrant-vbguest-in-china.html)


copy micromachine-2.0.0.gem  vagrant-share-1.1.9.gem  vagrant-vbguest-0.14.2.gem into ~/.vagrant.d/gems/2.3.4/cache


``` shell
$ vagrant plugin install vagrant-vbguest
```
