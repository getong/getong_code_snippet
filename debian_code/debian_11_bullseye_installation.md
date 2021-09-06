# debian 11 bullseye installation

## compile linux kernel

``` shell
# apt-get update
# apt-get upgrade -y
# apt-get install -y vim zsh curl nemo git moc moc-ffmpeg-plugin unzip p7zip-full w3m w3m-img proxychains
## in case of `OOM` error
# systemctl set-default multi-user.target
# reboot

# apt-get build-dep -y linux-source
# apt-get install -y firmware-linux firmware-linux-nonfree firmware-linux-free firmware-realtek libncurses6 libncurses-dev linux-source build-essential linux-headers-amd64 libelf-dev libssl-dev dwarves
# cd /usr/src
# xz -d -k linux-patch-5.10-rt.patch.xz
# tar xaf linux-source-5.10.tar.xz
# cd linux-source-5.10
# patch -p1 < ../linux-patch-5.10-rt.patch
# cp /boot/config-5.10.0-amd64 .config
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

## install wx

``` shell
apt-get install libwxgtk3.0-gtk3-dev libwxgtk-webview3.0-gtk3-dev
```


## update from buster to bullseye
change the `/etc/apt/source.list` to be:

```
deb http://mirrors.aliyun.com/debian/ bullseye main non-free contrib
deb-src http://mirrors.aliyun.com/debian/ bullseye main non-free contrib
deb http://mirrors.aliyun.com/debian-security bullseye/updates main
deb-src http://mirrors.aliyun.com/debian-security bullseye/updates main
deb http://mirrors.aliyun.com/debian/ bullseye-updates main non-free contrib
deb-src http://mirrors.aliyun.com/debian/ bullseye-updates main non-free contrib
deb http://mirrors.aliyun.com/debian/ bullseye-backports main non-free contrib
deb-src http://mirrors.aliyun.com/debian/ bullseye-backports main non-free contrib
```
and the run the commands:

``` shell
apt update
apt full-upgrade
```
Some packages need to manual comfirm the install option, for example, the sudoer, sshd_config, these files might be confirmed as we edit the files before.
copy from [Manually upgrade Debian from Buster to Bullseye](https://doc.akito.ooo/link/31#bkmrk-page-title)

## install kube-apiserver

``` shell
sudo apt update
sudo apt install snapd
sudo snap install core

sudo snap install kube-apiserver
```

copy from [Enable snaps on Debian and install kube-apiserver](https://snapcraft.io/install/kube-apiserver/debian)
