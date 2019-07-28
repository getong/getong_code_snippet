# buster amd64 installation

## upgrade the system

``` shell
# apt-get update
# apt-get upgrade -y
```
## add the /sbin to the root user PATH variable

``` shell
export PATH=/sbin:$PATH
```

## compile the linux kernel

``` shell
# apt-get build-dep -y linux
# apt-get install -y firmware-linux firmware-linux-nonfree firmware-linux-free
# cd /usr/src
# xz -d -k linux-patch-4.19-rt.patch.xz
# tar xaf linux-source-4.19.tar.xz
# cd linux-source-4.19
# patch -p1 < ../linux-patch-4.19-rt.patch
# cp /boot/config-4.19.0-5-amd64 .config
```
edit the .config file

```
CONFIG_SATA_PMP=n
#CONFIG_SYSTEM_TRUSTED_KEYS="debian/certs/debian-uefi-certs.pem"
```
and then compile and install the kernel

``` shell
# make menuconfig
# make -j`nproc` bzImage
# make -j`nproc` modules
# make modules_install
# make headers_install
# make install
# /sbin/shutdown -h now
```

## update-grub
edit the `/etc/default/grub`

```
GRUB_TIMEOUT=1
GRUB_CMDLINE_LINUX_DEFAULT="text"
GRUB_CMDLINE_LINUX="noapic acpi=off"
```
and then run the command:

``` shell
# update-grub
```

## static the network
check the network link name:

``` shell
$ ip a
```

edit the /etc/network/interfaces:
``` shell
auto enp2s0
allow-hotplug enp2s0
iface enp2s0 inet static
broadcast 192.168.1.255
address 192.168.1.100
netmask 255.255.255.0
gateway 192.168.1.1
dns-nameservers 192.168.1.1
```
disable the NetworkManager

``` shell
$ sudo systemctl disable NetworkManager
$ sudo systemctl stop NetworkManager
```

## install docker
``` shell
curl -fsSL https://get.docker.com | sudo bash -s docker --mirror Aliyun
```
add the current user to the docker group

``` shell
$ sudo usermod -aG docker $USER
```
