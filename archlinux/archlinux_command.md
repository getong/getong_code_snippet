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
    gnome gnome-extra gnome-tweak-tool gnome-shell grub efibootmgr efivar \
    intel-ucode proxychains v2ray asp git nemo emacs julia erlang \
    gnome-software-packagekit-plugin gnome-tweaks pacman-contrib \
    util-linux vagrant w3m wget xf86-video-nouveau xf86-video-intel mesa-libgl

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

sed -i 's/\#NAutoVTs=6/NAutoVTs=6/' /etc/systemd/logind.conf

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
SigLevel = Optional TrustAll
Server = http://mirrors.163.com/archlinux-cn/$arch
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

change the asp git remote url:

``` shell
$ cat ~/.cache/asp/.git/config
[core]
        repositoryformatversion = 0
        filemode = true
        bare = false
        logallrefupdates = true
[remote "packages"]
        url = https://github.com/archlinux/svntogit-packages.git
        fetch = +refs/heads/*:refs/remotes/packages/*
[remote "community"]
        url = https://github.com/archlinux/svntogit-community.git
        fetch = +refs/heads/*:refs/remotes/community/*
```
Change the packages url to be `https://gitee.com/mirrors_Archlinux/svntogit-packages`
change the community url to be `https://gitee.com/mirrors_Archlinux/svntogit-community`
The new file should be like this:

``` shell
$ cat ~/.cache/asp/.git/config

[core]
        repositoryformatversion = 0
        filemode = true
        bare = false
        logallrefupdates = true
[remote "packages"]
        url = https://gitee.com/mirrors_Archlinux/svntogit-packages
        fetch = +refs/heads/*:refs/remotes/packages/*
[remote "community"]
        url = https://gitee.com/mirrors_Archlinux/svntogit-community
        fetch = +refs/heads/*:refs/remotes/community/*

```
see [FS#67359 - [asp] remotes of existing ASPROOT are not automatically set to GitHub](https://bugs.archlinux.org/task/67359)


``` shell
 $ cd ~/
 $ mkdir build
 $ cd build/
 $ asp update linux
 $ asp export linux

 $ cd linux

 ## edit `PKGBUILD` file
 ------------------
 pkgbase=linux-custom

 ## change pkgname=("$pkgbase" "$pkgbase-headers" "$pkgbase-docs") to be:
 pkgname=("$pkgbase" "$pkgbase-headers")

 ## change https://github.com/archlinux/linux to be https://gitee.com/mirrors_Archlinux/linux
url="https://gitee.com/mirrors_Archlinux/linux/commits/$_srctag"
  "$_srcname::git+https://gitee.com/mirrors_Archlinux/linux?signed#tag=$_srctag"


------------------
change  the build() function
build() {
  cd $_srcname
  make all
  make htmldocs
}

to be :
build() {
  cd $_srcname
  make all -j$(nproc)
  make htmldocs
}

## git clone the linux source file into the src directory

``` shell
mkdir src/

git clone https://gitee.com/mirrors_ArchLinux/linux src/archlinux-linux
```

 ## then run the updpkgsums command
 $ mv config config.origin

 $ zcat /proc/config.gz > config

 ## edit `config` file
 ------------------
 CONFIG_SATA_PMP=n

 $ updpkgsums
 ## network might be broken, use proxy
 $ proxychains makepkg -s
```

copy from [Kernel (简体中文)/Arch Build System (简体中文)](https://wiki.archlinux.org/title/Kernel_(%E7%AE%80%E4%BD%93%E4%B8%AD%E6%96%87)/Arch_Build_System_(%E7%AE%80%E4%BD%93%E4%B8%AD%E6%96%87))
also see [Kernel/Arch Build System](https://wiki.archlinux.org/title/Kernel/Arch_Build_System)
also see [使用ABS编译ArchLinux内核](https://cloud.tencent.com/developer/article/1791129)

## git clone source and makepkg

``` shell

 $ asp update linux
 $ asp export linux
 $ cd linux
 $ git clone https://gitee.com/mirrors_Archlinux/linux archlinux-linux
 $ makepkg --holdver
```
copy from [How to modify a PKGBUILD which uses git sources to pull only a shallow clone?](https://unix.stackexchange.com/questions/154919/how-to-modify-a-pkgbuild-which-uses-git-sources-to-pull-only-a-shallow-clone)

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


## systemd-analyze

``` shell
systemd-analyze blame
systemd-analyze critical-chain
systemd-analyze plot > plot.svg
```
see [Improving performance/Boot process](https://wiki.archlinux.org/title/Improving_performance/Boot_process)

also see [Manjaro booting is very slow (40sec)](https://forum.manjaro.org/t/manjaro-booting-is-very-slow-40sec/32489)

## yay usage

``` shell
# Instalk dkms :
sudo pacman -Sy dkms

# Install Yay :
sudo pacman -Sy yay

# and run as USER:
yay -S nvidia-390xx-dkms
yay -S nvidia-340xx-dkms
```
copy from [Unable to find `nvidia-390xx` anymore](https://www.reddit.com/r/archlinux/comments/g4e6qq/unable_to_find_nvidia390xx_anymore/)
also see [Unsupported_drivers](https://wiki.archlinux.org/title/NVIDIA#Unsupported_drivers)

## install typora

``` shell
sudo pacman -S typora
```

## install virtualbox

``` shell
pacman -S virtualbox virtualbox-ext-oracle
```

## chinese font

``` shell
sudo pacman -S wqy-zenhei
sudo systemctl stop packagekit.service
```

## texlive

``` shell
sudo pacmn -S texlive-core texlive-latexextra
```

## install nfs-utils
``` shell
sudo pacman -S nfs-utils
```

## stop packagekit.service

``` shell
sudo systemctl stop packagekit.service
```
copy from [Gnome software store stopped working](https://bbs.archlinux.org/viewtopic.php?pid=1975599#p1975599)

## install google-chrome browser

``` shell
yay -S google-chrome
```

## fcitx

``` shell
sudo pacman -S fcitx-im fcitx fcitx-configtool fcitx-cloudpinyin
```
copy from [Arch安装和Gnome配置教程](https://hanielxx.com/Linux/2019-07-20-archLinux-gnome-install.html)

## pacman-key

``` shell
mv /etc/pacman.d/gnupg /etc/pacman.d/gnupg.bak

pacman-key --init
pacman-key --populate archlinux
pacman -Syyu
```

## kde

``` shell
sudo pacman -S plasma kde-applications
```

## pam fails to find unit dbus-org.freedesktop.home1.service

add this to /etc/pacman.conf
``` shell
NoExtract=usr/lib/security/pam_systemd_home.so
```
then reinstall systemd

``` shell
sudo pacman -S systemd
```

copy from [ pam fails to find unit dbus-org.freedesktop.home1.service](https://bbs.archlinux.org/viewtopic.php?pid=1927195#p1927195)

## mpv player

``` shell
sudo pacman -S mpv
```
copy from [VLC - process remains active after closing it](https://www.reddit.com/r/archlinux/comments/blo3zs/vlc_process_remains_active_after_closing_it/)
