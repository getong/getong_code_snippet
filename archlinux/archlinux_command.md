# archlinux command

## intallation
see [Archlinux 安装教程超详细（2021.11.15）](https://zhuanlan.zhihu.com/p/433920079)

``` shell
reflector --country China --age 72 --sort rate --protocol https --save /etc/pacman.d/mirrorlist

timedatectl set-ntp true

cfdisk /dev/sda

mkswap /dev/sda2

mount /dev/sda3 /mnt

pacstrap /mnt linux linux-firmware linux-headers base base-devel vim git bash-completion

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


pacman -S grub efibootmgr efivar networkmanager intel-ucode

grub-install /dev/sda
grub-mkconfig -o /boot/grub/grub.cfg

passwd

exit

umount /mnt

reboot

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
