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

## enable nonfree repo

``` shell
sudo apt-add-repository non-free
sudo apt-add-repository contrib

wget http://www.deb-multimedia.org/pool/main/d/deb-multimedia-keyring/deb-multimedia-keyring_2016.8.1_all.deb
echo "deb http://www.deb-multimedia.org stable main non-free"| sudo tee /etc/apt/sources.list.d/deb-multimedia.list
```
copy from [Installing Multimedia Codecs on Debian 10](https://linuxhint.com/install_multimedia_codecs_debian_10/)

## pavucontrol

``` shell
sudo apt-get install pavucontrol
pavucontrol
```
`配置` -> `内置音频` -> `模拟立体声双工`
`输出设备` -> `内置音频 模拟立体声` -> Port: Line Out

copy from [ubuntu18.04主机后置耳机没声音](https://forum.ubuntu.org.cn/viewtopic.php?t=487752)

## debian bullseye source mirror

``` shell
deb http://mirrors.163.com/debian/ bullseye main non-free contrib
deb http://mirrors.163.com/debian/ bullseye-updates main non-free contrib
deb http://mirrors.163.com/debian/ bullseye-backports main non-free contrib
deb-src http://mirrors.163.com/debian/ bullseye main non-free contrib
deb-src http://mirrors.163.com/debian/ bullseye-updates main non-free contrib
deb-src http://mirrors.163.com/debian/ bullseye-backports main non-free contrib
deb http://mirrors.163.com/debian-security/ bullseye-security main non-free contrib
deb-src http://mirrors.163.com/debian-security/ bullseye-security main non-free contrib
```

## gpt disk partition

``` shell
$ parted /dev/vdb
(parted) mktable gpt
(parted) print
(parted) mkpart
[ext2]? xfs
起始点？ 1
结束点？ 3TB
(parted) print
```
Not (MBR) msdos.
copy from [Linux下使用gpt给磁盘分区、格式化、挂载](https://blog.51cto.com/wangqh/2089129)

## btrfs

``` shell
sudo apt-get install btrfs-progs
```

## 32G swapfile
LLVM nees 27G memory, but many machines does have the amount of memory.
It can be solved by add swapfile:


``` shell
sudo dd if=/dev/zero of=/swapfile bs=1024 count=32768k
sudo mkswap /swapfile
sudo swapon /swapfile
sudo echo "/swapfile     swap    swap      defaults     0  0" >> /etc/fstab
sudo mount -a
```

copy from [创建交换文件（swapfile）-linux](https://blog.51cto.com/joket/1140156)

Also see [collect2: ld terminated with signal 9 错误解决办法](https://blog.csdn.net/longkg/article/details/12839173)

Also see [collect2: fatal error: ld terminated with signal 9 [Killed]](https://stackoverflow.com/questions/46259776/collect2-fatal-error-ld-terminated-with-signal-9-killed)

```
Increase SWAP disk (8 GB working with me).
Increase Memory (I was on virtual machine, 8 GB).
You need 27 GB (LLVM 6.0.0) free disk space (check with df -h in Terminal).
```

The cargo command:

``` shell
 cargo run --release --verbose --jobs 1
 cargo build --jobs 1
```
