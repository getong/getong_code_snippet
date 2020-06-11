# vagrant

## vagrant boxes
[Discover Vagrant Boxes](https://app.vagrantup.com/boxes/search)

## update box

``` shell
mkdir centos8
cd centos8
vagrant init generic/centos8
vagrant up
vagrant box update
```

## Vagrant was unable to mount VirtualBox shared folders.

``` shell
vagrant plugin install vagrant-vbguest
vagrant reload
```
copy from [Vagrant was unable to mount VirtualBox shared folders](https://mecromace.com/articles/2019/11/vagrant-was-unable-mount-virtualbox-shared-folders)
