# buster amd64 installation

## upgrade the system

``` shell
# apt-get update
# apt-get upgrade -y
# apt-get install vim zsh curl git -y
```
## add the /sbin to the root user PATH variable

``` shell
export PATH=/sbin:$PATH
```

## compile the linux kernel

``` shell
# apt-get build-dep -y linux
# apt-get install -y firmware-linux firmware-linux-nonfree firmware-linux-free firmware-realtek
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
disable the NetworkManager, and enable networking

``` shell
$ sudo systemctl disable NetworkManager
$ sudo systemctl stop NetworkManager
$ sudo systemctl enable networking
```

## install docker
``` shell
curl -fsSL https://get.docker.com | sudo bash -s docker --mirror Aliyun
```
add the current user to the docker group

``` shell
$ sudo usermod -aG docker $USER
```

## trash-cli setting

``` shell
sudo rm -rf /[partition mount]/.Trash-`id -u user_name`
sudo mkdir /[partition mount]/.Trash-`id -u user_name`
sudo chown user_name:user_group /[partition mount]/.Trash-`id -u user_name`
```

## fcitx installation
```shell
# apt-get install fcitx-ui-classic fcitx-frontend-fbterm fcitx-frontend-gtk3 im-switch
# sudo chmod u+s /usr/bin/fbterm
# im-config
```
set the fcitx as the default input method
Edit ~/.fbtermrc
```shell
input-method=fcitx-fbterm
```
## ifup

``` shell
sudo ifup enp2s0
```
## check the physical cable is up and its state

``` shell
for i in $(ls /sys/class/net)
do
echo $i
cat /sys/class/net/$i/carrier
cat /sys/class/net/$i/operstate
done
```
copy from [How to detect whether a physical cable is connected to network card slot on Linux](https://linuxconfig.org/how-to-detect-whether-a-physical-cable-is-connected-to-network-card-slot-on-linux)

## Network Manager status
``` shell
nmcli dev status
```

## network tool

``` shell
sudo apt install net-tools lshw ethtool
sudo lshw -class network -short
sudo ethtool enp2s0
sudo ethtool -i enp2s0
ip a s enp2s0
```

## rename

``` shell
sudo apt-get install rename inotify-tools jigdo-file calibre
```

## ifdown and ifup

``` shell
sudo systemctl stop ModemManager
sudo systemctl disable ModemManager
sudo ifdown enp2s0
sudo ifup enp2s0
sudo systemctl restart networking
```
