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

## vagrant mount usb device

see [USB device is not visible inside Vagrant](https://stackoverflow.com/questions/38956127/usb-device-is-not-visible-inside-vagrant)
see [Connect a Usb device through Vagrant](https://code-chronicle.blogspot.com/2014/08/connect-usb-device-through-vagrant.html)
see [VirtualBox USB passthrough](https://softwaretester.info/category/development/virtualization/)

commands:

``` shell
$ VBoxManage list usbhost
UUID:               e007f0f3-c498-495a-ac02-d88a92831bfb
VendorId:           0x0930 (0930)
ProductId:          0x1407 (1407)
Revision:           16.117 (16117)
Port:               3
USB version/speed:  0/Super
Manufacturer:       TOSHIBA
Product:            TransMemory-Ex2
SerialNumber:       60A44CB464581F40A35A00BB
Address:            p=0x1407;v=0x0930;s=0x0000003b7c753fcd;l=0x01133000
Current State:      Busy

```
Change the following to the `Vagrantfile` file:
``` shell
  config.vm.provider "virtualbox" do |vb|
  #   # Display the VirtualBox GUI when booting the machine
  #  vb.gui = true
  #
  #   # Customize the amount of memory on the VM:
  #   vb.memory = "1024"
    vb.customize ["modifyvm", :id, "--usb", "on"]
    vb.customize ["modifyvm", :id, "--usbxhci", "on"]
    vb.customize ["usbfilter", "add", "0",
                  "--target", :id,
                  "--manufacturer", "TOSHIBA",
                  "--name", "toshiba",
                  "--product", "TransMemory-Ex2"]
  end
```

Then run the virtualbox virtual machine:

``` shell
$ vagrant up
```
Run into the virtual machine:

``` shell
$ vagrant ssh
$ sudo -i
# mkdir -p /media/vagrant
# fdisk -l
# mount /dev/sdb1 /media/vagrant
# cp * /vagrant
# cd
# umount /media/vagrant
```

## virtualbox add vram

``` shell
VBoxManage modifyvm "name_of_vm" --vram 256
```
The max vram is 256m.

## VAGRANT_HOME

``` shell
export VAGRANT_HOME=/backup/vagrant_boxes
```
copy from [Where does Vagrant download its .box files to?](https://stackoverflow.com/questions/10155708/where-does-vagrant-download-its-box-files-to)

## transfer virtualbox image into vagrant box image

``` shell
vagrant package --base [machine name as it shows in virtual box] --output /Users/myuser/Documents/Workspace/my.box

// copy the box to your remote

vagrant init [machine name as it shows in virtual box] /Users/myuser/Documents/Workspace/my.box

vagrant up
```
copy from [How to export a Vagrant virtual machine to transfer it](https://stackoverflow.com/questions/20679054/how-to-export-a-vagrant-virtual-machine-to-transfer-it)


## package vagrant instance into box

``` shell
// cd vagrant instance directory
vagrant package
```
