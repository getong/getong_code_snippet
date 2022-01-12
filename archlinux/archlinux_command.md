# archlinux command

## intallation
see [Archlinux 安装教程超详细（2021.11.15）](https://zhuanlan.zhihu.com/p/433920079)
see [How to Install Arch Linux in 2022 {Easy Step-by-Step Guide}](https://www.securedyou.com/how-to-install-arch-linux-step-by-step-tutorial/)
see [Archlinux 2022安装配置之Gnome40](https://www.cnblogs.com/LzsCxb/p/15669736.html)
``` shell
reflector --country China --age 72 --sort rate --protocol https --save /etc/pacman.d/mirrorlist

timedatectl set-ntp true

cfdisk /dev/sda

mkswap /dev/sda2

mount /dev/sda3 /mnt

pacstrap /mnt linux linux-firmware linux-headers base base-devel vim git \
    bash-completion net-tools openssh gdm xorg xorg-server xorg-xinit xorg-xrandr \
    gnome gnome-extra gnome-tweak-tool gnome-shell chrome-gnome-shell \
    grub efibootmgr efivar intel-ucode proxychains v2ray asp git \
    nemo emacs julia erlang gnome-software-packagekit-plugin gnome-tweaks

genfstab -U /mnt >> /mnt/etc/fstab

arch-chroot /mnt

ln -sf /usr/share/zoneinfo/Asia/Shanghai /etc/localtime
hwclock --systohc

vim /etc/locale.gen
en_US.UTF-8 UTF-8
zh_CN.UTF-8 UTF-8

locale-gen


echo "LANG=en_US.UTF-8" > /etc/locale.conf
echo archlinux > /etc/hostname


vim /etc/hosts
--------------------
127.0.0.1   localhost
::1         localhost
127.0.0.1   archlinux.localdomain archlinux   # 这里的archlinux是主机名

grub-install /dev/sda

vim /etc/default/grub
--------------------
GRUB_TIMEOUT=1
GRUB_CMDLINE_LINUX_DEFAULT="text"
# GRUB_CMDLINE_LINUX="noapic acpi=off"

grub-mkconfig -o /boot/grub/grub.cfg

passwd

useradd --create-home user_name
passwd user_name

usermod -aG wheel,users,storage,power,lp,adm,optical user_name

vim /etc/sudoers
## right under the %wheel ALL=(ALL) NOPASSWD: ALL
$USER  ALL=(ALL) NOPASSWD:ALL


systemctl enable sshd

vim /etc/systemd/network/static-enp1s0.network
-------------------
[Match]
Name=enp1s0
[Network]
Address=192.168.1.81/24
Gateway=192.168.1.253
DNS=192.168.253.254
DNS=192.168.1.253


vim /etc/systemd/network/static-enp1s0.link
--------------------------------
[Match]
MACAddress=a8:4b:05:2b:e8:54

[Link]
NamePolicy=
Name=enp1s0

rmmod pcspkr
echo "blacklist pcspkr" >> /etc/modprobe.d/blacklist.conf

exit

umount /mnt

reboot

```

## login and enable the network

``` shell
systemctl enable systemd-networkd

systemctl enable systemd-resolved.service

systemctl start gdm.service
systemctl enable gdm.service
```

## add user and set group

``` shell
useradd --create-home user_name
passwd user_name

usermod -aG wheel,users,storage,power,lp,adm,optical user_name

```

## add archlinuxcn

``` shell
vim /etc/pacman.conf
--------------------------------------

[archlinuxcn]
Server = https://mirrors.ustc.edu.cn/archlinuxcn/$arch
```

## install archlinuxcn-keyring

``` shell
pacman -S archlinuxcn-keyring
```

## install yay

``` shell
pacman -S yay
```

## set grub and update grub

``` shell
vim /etc/default/grub

grub-mkconfig
```

## install fbterm

``` shell
yay -S fbterm
```

## set the mirror and update

``` shell
cd /etc/pacman.d/
sudo mv mirrorlist  mirrorlist.bak
echo "Server = http://mirrors.163.com/archlinux/$repo/os/$arch" | sudo tee  mirrorlist
sudo pacman -Syu
```

## vagrant box

``` shell
vagrant init ogarcia/archlinux-x64
vagrant up
```

## asp compile kernel

``` shell
 $ cd ~/
 $ mkdir build
 $ cd build/
 $ asp update linux
 $ asp export linux

 $ cd linux

 ## edit PKGBUILD
 pkgbase=linux-custom

 ## change pkgname=("$pkgbase" "$pkgbase-headers" "$pkgbase-docs") to be:
 pkgname=("$pkgbase" "$pkgbase-headers")

 ## edit config
 CONFIG_SATA_PMP=n

 ## then run the updpkgsums command
 $ updpkgsums
 $ makepkg -s
```

copy from [Kernel (简体中文)/Arch Build System (简体中文)](https://wiki.archlinux.org/title/Kernel_(%E7%AE%80%E4%BD%93%E4%B8%AD%E6%96%87)/Arch_Build_System_(%E7%AE%80%E4%BD%93%E4%B8%AD%E6%96%87))
also see [Kernel/Arch Build System](https://wiki.archlinux.org/title/Kernel/Arch_Build_System)

## install software

``` shell
sudo pacman -S proxychains vim v2ray
```

## archlinux-linux git repo ... FAILED (unknown public key 3B94A80E50A477C7)

``` shell
gpg --keyserver hkps://keys.openpgp.org  --recv-keys 3B94A80E50A477C7
```
copy from [[SOLVED] Unknown public key error while building linux kernel](https://bbs.archlinux.org/viewtopic.php?id=268750)


## install gnome desktop

``` shell
pacman -S xorg-server xorg-xinit gnome gnome-extra
```

copy from [ArchLinux安装GNOME图形桌面环境](https://starrycat.me/archlinux-install-gnome-desktop.html)

## install via ssh remotely

``` shell
reflector --country China --age 72 --sort rate --protocol https --save /etc/pacman.d/mirrorlist

pacman -Syy

## add password of root user
passwd

pacman -S net-tools openssh

systemctl start sshd

pacman -S archinstall


```
copy from [通过ssh远程安装arch linux ](https://blog.51cto.com/u_3258791/2097197）


## blame

``` shell
systemd-analyze blame
systemd-analyze critical-chain
systemd-analyze plot > plot.svg
```
see [Improving performance/Boot process](https://wiki.archlinux.org/title/Improving_performance/Boot_process)

also see [Manjaro booting is very slow (40sec)](https://forum.manjaro.org/t/manjaro-booting-is-very-slow-40sec/32489)
