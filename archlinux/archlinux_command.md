# archlinux command

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
