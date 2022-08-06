# archlinux command

## intallation
see [Archlinux 安装教程超详细（2021.11.15）](https://zhuanlan.zhihu.com/p/433920079)

see [How to Install Arch Linux in 2022 {Easy Step-by-Step Guide}](https://www.securedyou.com/how-to-install-arch-linux-step-by-step-tutorial/)

see [Archlinux 2022安装配置之Gnome40](https://www.cnblogs.com/LzsCxb/p/15669736.html)

``` shell
# reflector --country China --age 72 --sort rate --protocol https --save /etc/pacman.d/mirrorlist

vim /etc/pacman.d/mirrorlist
-------------------------------
## delete all other mirrors
Server = http://mirrors.163.com/archlinux/$repo/os/i686

timedatectl set-ntp true

cfdisk /dev/sda


// none efi
mount /dev/sda3 /mnt

// efi option, EFI system partition
mount /dev/nvme0n1p2 /mnt
mkdir /mnt/boot
mount /dev/nvme0n1p1 /mnt/boot

// mount other part disk
mkdir /mnt/backup
mount /dev/sda2 /mnt/backup

pacstrap /mnt linux linux-firmware linux-headers base base-devel vim git \
    net-tools openssh gdm xorg xorg-server xorg-xinit xorg-xrandr \
    gnome gnome-extra gnome-tweak-tool gnome-shell grub efibootmgr \
    intel-ucode proxychains v2ray asp nemo emacs julia erlang \
    gnome-software-packagekit-plugin gnome-tweaks pacman-contrib \
    util-linux vagrant w3m wget xf86-video-nouveau xf86-video-intel mesa-libgl \
    wqy-zenhei cmake reflector bc nano zsh trash-cli vlc cpio xmlto python-sphinx_rtd_theme \
    virtualbox virtualbox-host-dkms virtualbox-guest-iso \
    adobe-source-han-serif-cn-fonts noto-fonts-cjk noto-fonts-emoji noto-fonts-extra \
    adobe-source-han-sans-cn-fonts ttf-sarasa-gothic texlive-core texlive-latexextra \
    nfs-utils mpv wpa_supplicant samba pandoc texlive-fontsextra texlive-langchinese \
    gst-libav a52dec faac faad2 flac jasper lame libdca libdv libmad libmpeg2 \
    libtheora libvorbis libxv wavpack x264 xvidcore \
    fcitx-configtool fcitx fcitx-gtk3 sunpinyin fcitx-sunpinyin calibre gthumb \
    moc firefox firefox-i18n-zh-cn p7zip tmux

genfstab -U /mnt >> /mnt/etc/fstab
// after genfstab , if `/mnt/boot` is mounted, check the `/mnt/etc/fstab`
// if the `/mnt/etc/fstab` file  does not contain `/mnt/boot`, it must be installed error.

arch-chroot /mnt

cat >>/etc/pacman.conf <<EOF
[archlinuxcn]
Server = https://mirrors.ustc.edu.cn/archlinuxcn/\$arch
EOF

pacman -Syyu
pacman -S --noconfirm archlinuxcn-keyring

pacman -S --noconfirm virtualbox-ext-oracle gstreamer0.10-base-plugins netease-cloud-music authy vdhcoapp feishu-bin

ln -sf /usr/share/zoneinfo/Asia/Shanghai /etc/localtime
hwclock --systohc

# locale
sed --in-place=.bak 's/^#en_US\.UTF-8/en_US\.UTF-8/' /etc/locale.gen
sed --in-place=.bak2 's/^#zh_CN\.UTF-8/zh_CN\.UTF-8/' /etc/locale.gen
locale-gen


echo "LANG=zh_CN.UTF-8" > /etc/locale.conf
echo archlinux > /etc/hostname

echo >> /etc/hosts <<EOF
--------------------
127.0.0.1   localhost
::1         localhost
127.0.0.1   archlinux.localdomain archlinux   # 这里的archlinux是主机名
EOF

// optional, check the /boot is installed
mkinitcpio -P

// no efi, grub-install --recheck /dev/<目标磁盘>
grub-install /dev/sda

// efi option
// grub-install --target=x86_64-efi --efi-directory=<EFI 分区挂载点> --bootloader-id=arch_grub --recheck
grub-install --target=x86_64-efi --efi-directory=/boot --bootloader-id=arch_grub --recheck

mkdir /boot/EFI/boot
cp /boot/EFI/arch_grub/grubx64.efi  /boot/EFI/boot/bootx64.efi
grub-mkconfig -o /boot/grub/grub.cfg

// checkout the menuentry
grep menuentry /boot/grub/grub.cfg

vim /etc/default/grub
--------------------
GRUB_TIMEOUT=1
GRUB_CMDLINE_LINUX_DEFAULT="text"
GRUB_CMDLINE_LINUX="noapic"

grub-mkconfig -o /boot/grub/grub.cfg

passwd

useradd --create-home user_name
passwd user_name

usermod -aG wheel,users,storage,power,lp,adm,optical user_name

vim /etc/sudoers
## right under the %wheel ALL=(ALL) NOPASSWD: ALL
$USER  ALL=(ALL) NOPASSWD:ALL


systemctl enable sshd

// static ip
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

// dhcp
vim /etc/systemd/network/dhcp-enp1s0.network
-------------------
[Match]
Name=enp1s0
[Network]
DHCP=ipv4

rmmod pcspkr
echo "blacklist pcspkr" >> /etc/modprobe.d/blacklist.conf

sed -i 's/\#NAutoVTs=6/NAutoVTs=6/' /etc/systemd/logind.conf

systemctl enable systemd-networkd

systemctl enable systemd-resolved.service

exit

umount /mnt

reboot

```

## login and enable the network

``` shell
sudo systemctl enable systemd-networkd

sudo systemctl enable systemd-resolved.service

sudo systemctl start gdm.service
// systemctl enable gdm.service

sudo systemctl set-default graphical
```

## add user and set group

``` shell
useradd --create-home gerald
passwd gerald

usermod -aG wheel,users,storage,power,lp,adm,optical gerald

```

## add archlinuxcn

``` shell
vim /etc/pacman.conf
--------------------------------------

[archlinuxcn]
Server = https://mirrors.163.com/archlinux-cn/$arch
```

## install archlinuxcn-keyring

``` shell
pacman -Syyu
pacman -S --noconfirm archlinuxcn-keyring
```

## install yay

``` shell
pacman -S  --noconfirm yay
```

## set grub and update grub

``` shell
vim /etc/default/grub

grub-mkconfig
```

## install fbterm

``` shell
yay -S --noconfirm fbterm fbv fcitx-fbterm-git v86d

sudo gpasswd -a $USER video

sudo setcap 'cap_sys_tty_config+ep' /usr/bin/fbterm

sudo chmod u+s /usr/bin/fbterm
sudo pacman -S fbgrab
```
fbterm usage:

``` shell
       keyboard:
         CTRL_ALT_E:    exit from FbTerm
         CTRL_ALT_C:    create a new window
         CTRL_ALT_D:    destroy current window
         CTRL_ALT_1:    switch to window 1
         CTRL_ALT_2:    switch to window 2
         CTRL_ALT_3:    switch to window 3
         CTRL_ALT_4:    switch to window 4
         CTRL_ALT_5:    switch to window 5
         CTRL_ALT_6:    switch to window 6
         CTRL_ALT_7:    switch to window 7
         CTRL_ALT_8:    switch to window 8
         CTRL_ALT_9:    switch to window 9
         CTRL_ALT_0:    switch to window 10
         SHIFT_LEFT:    switch to previous window
         SHIFT_RIGHT:   switch to next window
         SHIFT_PAGEUP:    history scroll up
         SHIFT_PAGEDOWN:  history scroll down
         CTRL_ALT_F1:                 switch to encoding of current locale
         CTRL_ALT_F2 to CTRL_ALT_F6:  switch to additional encodings
         CTRL_SPACE:    toggle input method
         CTRL_ALT_K:    kill input method server
```
copy from [fbterm](http://manpages.ubuntu.com/manpages/bionic/man1/fbterm.1.html)

fbterm start with font size:

``` shell
fbterm -s 40
```

## set the mirror and update

``` shell
cd /etc/pacman.d/
sudo mv mirrorlist  mirrorlist.bak
echo 'Server = http://mirrors.163.com/archlinux/$repo/os/$arch' | sudo tee  mirrorlist
sudo pacman -Syyu
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

 // edit `PKGBUILD` file
 ------------------
 pkgbase=linux-custom

 // change pkgname=("$pkgbase" "$pkgbase-headers" "$pkgbase-docs") to be:
 ------------------------------------------------------------------
 pkgname=("$pkgbase" "$pkgbase-headers")

 // change https://github.com/archlinux/linux to be https://gitee.com/mirrors_Archlinux/linux
 ----------------------------------------------------------------------
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

``` shell

 $ mv config config.origin

 $ zcat /proc/config.gz > config

 ## edit `config` file
 ------------------
 CONFIG_SATA_PMP=n

 ## edit `PKGBUILD` file
 ------------------
 pkgbase=linux-custom

 ## change pkgname=("$pkgbase" "$pkgbase-headers" "$pkgbase-docs") to be:
 pkgname=("$pkgbase" "$pkgbase-headers")

 ## ------------------
## change  the build() function
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


 $ updpkgsums
 # network might be broken, use proxy
 $ proxychains makepkg -s

// clean up
$ cd ~/build/linux/src/archlinux
$ git branch --delete --force --verbose 5.16.11-arch1-1
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

 $ mv config config.origin

 $ zcat /proc/config.gz > config

 ## edit `config` file
 ------------------
 CONFIG_SATA_PMP=n


 // checkout a tag like 5.16.11-arch1
 $ git branch --delete --force --verbose 5.16.11-arch1
 $ git checkout tags/v5.16.11-arch1 -b 5.16.11-arch1

 $ rm -rf pkg src  ## there might be old building directory

 $ updpkgsums
 $ makepkg --holdver
 $ sudo pacman -U <kernel-headers_package>
 $ sudo pacman -U <kernel_package>
 $ sudo grub-mkconfig -o /boot/grub/grub.cfg

 // uninstall custom linux kernel
 $ sudo pacman -R linux-custom-headers
 $ sudo pacman -R linux-custom
 $ sudo grub-mkconfig -o /boot/grub/grub.cfg
```
copy from [How to modify a PKGBUILD which uses git sources to pull only a shallow clone?](https://unix.stackexchange.com/questions/154919/how-to-modify-a-pkgbuild-which-uses-git-sources-to-pull-only-a-shallow-clone)

## install software

``` shell
sudo pacman -S proxychains vim v2ray
```

## archlinux-linux git repo ... FAILED (unknown public key 3B94A80E50A477C7)

``` shell
gpg --keyserver hkps://keys.openpgp.org  --recv-keys 3B94A80E50A477C7

// or
wget -c https://keys.openpgp.org/vks/v1/by-fingerprint/A2FF3A36AAA56654109064AB19802F8B0D70FC30 https://keys.openpgp.org/vks/v1/by-fingerprint/C7E7849466FE2358343588377258734B41C31549
gpg --import A2FF3A36AAA56654109064AB19802F8B0D70FC30
gpg --import C7E7849466FE2358343588377258734B41C31549
```
copy from [[SOLVED] Unknown public key error while building linux kernel](https://bbs.archlinux.org/viewtopic.php?id=268750)


## install gnome desktop

``` shell
sudo pacman -S xorg-server xorg-xinit gnome gnome-extra
```

copy from [ArchLinux安装GNOME图形桌面环境](https://starrycat.me/archlinux-install-gnome-desktop.html)

## install via ssh remotely

``` shell
reflector --country China --age 72 --sort rate --protocol https --save /etc/pacman.d/mirrorlist

sudo pacman -Syy

## add password of root user
passwd

sudo pacman -S net-tools openssh glibc

systemctl start sshd

sudo pacman -S archinstall

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

sudo cp /usr/share/nvidia-340xx/20-nvidia.conf /etc/X11/xorg.conf.d/
```
copy from [Unable to find `nvidia-390xx` anymore](https://www.reddit.com/r/archlinux/comments/g4e6qq/unable_to_find_nvidia390xx_anymore/)
also see [Unsupported_drivers](https://wiki.archlinux.org/title/NVIDIA#Unsupported_drivers)

## nvidia

``` shell
sudo pacman -S viadia
```
For the Maxwell (NV110/GMXXX) series and newer, install the nvidia package
copy from [NVIDIA](https://wiki.archlinux.org/title/NVIDIA)

GeForce 930起、10系至20系、 Quadro/Tesla/Tegra K-系列以及更新的显卡（NV110以及更新的显卡家族），安装 nvidia （用于linux） 或者 nvidia-lts （用于linux-lts）。
copy from [NVIDIA (简体中文)](https://wiki.archlinux.org/title/NVIDIA_(%E7%AE%80%E4%BD%93%E4%B8%AD%E6%96%87))

## install typora

``` shell
sudo pacman -S typora
```

## install virtualbox

``` shell
sudo pacman -S virtualbox virtualbox-host-dkms virtualbox-guest-iso \
    virtualbox-ext-oracle

sudo gpasswd -a $USER vboxusers

sudo systemctl enable --now vboxweb.service

echo -e "vboxdrv\nvboxnetflt\nvboxnetadp\nvboxpci" | sudo tee -a /etc/modules-load.d/virtualbox.conf

sudo vboxreload

// or setup
sudo /sbin/rcvboxdrv setup
```
copy from [Arch Linux 安装 virtualbox_powerx_yc的博客-程序员宝宝](https://cxybb.com/article/weixin_34280237/91997886)

## chinese font

``` shell
sudo pacman -S wqy-zenhei adobe-source-han-serif-cn-fonts

sudo pacman -S noto-fonts-cjk noto-fonts-emoji noto-fonts-extra adobe-source-han-sans-cn-fonts adobe-source-han-serif-cn-fonts  ttf-sarasa-gothic
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

## pacman-key

``` shell
mv /etc/pacman.d/gnupg /etc/pacman.d/gnupg.bak

pacman-key --init
pacman-key --populate archlinux
sudo pacman -Syyu
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

## ugrren ac1300 wireless card

``` shell
git clone https://github.com/morrownr/8812au-20210629
cd 8812au-20210629
sudo ./install-driver.sh
sudo reboot
```
copy from [Package Details: rtl88xxau-aircrack-dkms-git r1174.3a6402e-1](https://aur.archlinux.org/packages/rtl88xxau-aircrack-dkms-git)

## some useful command:

``` shell
lspci -k // 检查驱动状态
ip link
iw dev // 以上三种方法都能列出网卡接口名称，如果未列出，说明驱动有问题。

dmesg | grep firmware
dmesg | grep iwlwifi // 这两种方法是在驱动有问题时，可以用来检查和寻找问题。

iw dev wlp3s0 link // 用来检查网卡是否连接，其中 wlp3s0 是网卡接口名称

ip link set dev wlp3s0 up // 用来将无线网卡接口启用

iw dev wlp3s0 scan // 用来扫描周围的无线热点

iw dev wlp3s0 connect "your_essid" // 此命令可以直接连接没有加密的热点
iw dev wlp3s0 connect "your_essid" key 0:your_key // 用来连接WEP加密的热点

wpa_supplicant -D nl80211,wext -B -i wlp3s0 -c /etc/wpa_supplicant/wpa_supplicant.conf
// 这是 wpa_supplicant 连接WPA/WPA2的命令格式

dhcpcd wlp3s0 // 给无线网卡分配动态IP
```
copy from [archlinux 系统安装无线网卡](https://wangbinweb.github.io/htm/18-archlinux-install-wireless-network-card.htm)
also see [Network configuration (简体中文)/Wireless (简体中文)](https://wiki.archlinux.org/title/Network_configuration_(%E7%AE%80%E4%BD%93%E4%B8%AD%E6%96%87)/Wireless_(%E7%AE%80%E4%BD%93%E4%B8%AD%E6%96%87))

## ctrl swap caps

Get the option:

``` shell
grep "caps" /usr/share/X11/xkb/rules/xorg.lst
```

add following line to ~/.xinitrc or ~/.xsession, before the exec gnome-session (or similar) line .

``` shell
setxkbmap -option ctrl:swapcaps
```
copy from [Remapping Caps Lock to Esc on Arch Linux](https://n1ghtmare.github.io/2021-05-19/remapping-caps-lock-to-esc-on-arch-linux/)
also see [Linux – How to map the Caps Lock key to Escape key in Arch Linux](https://itectec.com/superuser/how-to-map-the-caps-lock-key-to-escape-key-in-arch-linux/)

## samba

``` shell
sudo pacman -S samba

sudo vim /etc/samba/smb.conf
# ----------------------------- 　　添加以下内容
[global]
    dns proxy = No
    map to guest = Bad User
    netbios name = ARCH LINUX
    security = USER
    server string = Samba Server %v
    idmap config * : backend = tdb


[public]
    guest ok = Yes
    path = /home/test/shares
    read only = No


[private]
    path = /home/test/privates
    read only = No
    write list = @test

#--------------------------------

sudo mkdir /home/test
sudo mkdir /home/test/shares

sudo mkdir /home/test/privates

sudo chmod 777 /home/test/shares

sudo chmod 777 /home/test/privates

sudo groupadd smbgroup

sudo useradd -g smbgroup test

sudo smbpasswd -a test


sudo vim /etc/ssh/sshd_config
------------------------
AllowUsers  root@192.168.1.1 valid_user  # no test user here, test user is not allowed to login

sudo usermod --shell /usr/bin/nologin --lock test


sudo systemctl start smb
sudo systemctl enable smb

sudo systemctl restart sshd
```
see [Arch Linux下配置Samba](https://www.cnblogs.com/chenyucong/p/8452770.html)
see [Samba (简体中文)](https://wiki.archlinux.org/title/Samba_(%E7%AE%80%E4%BD%93%E4%B8%AD%E6%96%87))
see [Linux – 限制ssh的登录用户和登录ip](https://www.xiebruce.top/1089.html)

## mount external disk drivers
in the `/etc/fstab`

``` shell
UUID=177ce77f-6e06-47a6-b1e9-6c3a6b43fb8d /run/media/sinux/sinux3       ext4    noatime,x-systemd.mount-timeout=5min,x-systemd.automount,x-systemd.device-timeout=10,x-systemd.idle-timeout=1min 0 2
// or
UUID=XXXXXXXXXXXXXXX  /myfs btrfs defaults,auto,nofail,x-systemd.device-timeout=30,x-systemd.mount-timeout=30 0 0
```
A couple things:

1 You are missing a 2 at the end of the line for sinux3
2 You should use systemd-automounts to mount external drives. That way it doesn’t matter if they are connected or not.
It won’t impact your boot and you won’t have manually mount them later.
copy from [Dependency failed for File System Check External Drives](https://forum.endeavouros.com/t/dependency-failed-for-file-system-check-external-drives/16249)
copy from [Mount an external drive at boot time only if it is plugged in](https://askubuntu.com/questions/14365/mount-an-external-drive-at-boot-time-only-if-it-is-plugged-in)

## grub acpi
``` shell
GRUB_CMDLINE_LINUX="noapic acpi=off"
GRUB_CMDLINE_LINUX_DEFAULT="i8042.nomux=1 i8042.reset"

// or
GRUB_CMDLINE_LINUX_DEFAULT="i8042.nomux=1"

// or
GRUB_CMDLINE_LINUX_DEFAULT="i8042.reset i8042.nomux i8042.nopnp i8042.noloop"
```
see [What does the 'i8042.nomux=1' kernel option do during booting of Ubuntu?](https://unix.stackexchange.com/questions/28736/what-does-the-i8042-nomux-1-kernel-option-do-during-booting-of-ubuntu)
also see [Keyboard issue on Asus UM425UAZ](https://forums.linuxmint.com/viewtopic.php?t=356420)
also see [Can't pass the acpi=off problem](https://askubuntu.com/questions/929904/cant-pass-the-acpi-off-problem)

## ntp

``` shell
sudo pacman -S ntp

sudo ntpdate pool.ntp.org
sudo hwclock -w

sudo timedatectl set-ntp true
```
also see [systemd-timesyncd (简体中文)](https://wiki.archlinux.org/title/Systemd-timesyncd_(%E7%AE%80%E4%BD%93%E4%B8%AD%E6%96%87))

## Disable the grub sub-menu
edit the file `/etc/default/grub`
``` shell
GRUB_DEFAULT=saved
GRUB_SAVEDEFAULT=true
GRUB_DISABLE_SUBMENU=y
```
Then run the command:
``` shell
sudo grub-mkconfig -o /boot/grub/grub.cfg
```

## clean archlinux

``` shell
// Clean package cache
sudo pacman -Sc
sudo pacman -Scc
sudo pacman -S pacman-contrib

sudo vim /etc/systemd/system/paccache.timer
-----------------------------
[Unit]
Description=Clean-up old pacman pkg

[Timer]
OnCalendar=monthly
Persistent=true

[Install]
WantedBy=multi-user.target
-----------------------------

sudo systemctl enable paccache.timer
sudo systemctl start paccache.timer


//  Remove unused packages (orphans)
sudo pacman -Qtdq
sudo pacman -Rns $(pacman -Qtdq)


// Remove duplicates, empty files, empty directories and broken symlinks
sudo pacman -S rmlint
rmlint /home/alu

```
copy from [How to clean Arch Linux](https://averagelinuxuser.com/clean-arch-linux/)

## delete linux kernel
The -s flag will remove unused linux dependencies as well.
``` shell
sudo pacman -Ss linux
sudo pacman -Rs linux
```
copy from [Can I get rid off unused linux kernels?](https://www.reddit.com/r/archlinux/comments/mnbv18/can_i_get_rid_off_unused_linux_kernels/)

## Set default kernel in GRUB using grub-set-default command line

``` shell
grep menuentry /boot/grub/grub.cfg
```
You'll see each kernel listed with the name that is shown in the GRUB boot menu. The first one is 0, the second is 1, and so on.

``` shell
sudo grub-set-default X
```
where X is the number of the kernel you want to boot into.

Also set the X in the `/etc/default/grub` file

``` shell
GRUB_DEFAULT=X
```
and then running

``` shell
sudo grub-mkconfig -o /boot/grub/grub.cfg

// in some distribution
sudo update-grubas
```
copy from [Set default kernel in GRUB](https://unix.stackexchange.com/questions/198003/set-default-kernel-in-grub)

## Set default kernel in GRUB manually

1) Find the $menuentry_id_option for the submenu:
``` shell
$ grep submenu /boot/grub/grub.cfg
submenu 'Advanced options for Debian GNU/Linux' $menuentry_id_option 'gnulinux-advanced-38ea4a12-6cfe-4ed9-a8b5-036295e62ffc' {
```

2) Find the $menuentry_id_option for the menu entry for the kernel you want to use:

``` shell
$ grep gnulinux /boot/grub/grub.cfg
menuentry 'Debian GNU/Linux' --class debian --class gnu-linux --class gnu --class os $menuentry_id_option 'gnulinux-simple-38ea4a12-6cfe-4ed9-a8b5-036295e62ffc' {
submenu 'Advanced options for Debian GNU/Linux' $menuentry_id_option 'gnulinux-advanced-38ea4a12-6cfe-4ed9-a8b5-036295e62ffc' {
    menuentry 'Debian GNU/Linux, with Linux 4.18.0-0.bpo.1-rt-amd64' --class debian --class gnu-linux --class gnu --class os $menuentry_id_option 'gnulinux-4.18.0-0.bpo.1-rt-amd64-advanced-38ea4a12-6cfe-4ed9-a8b5-036295e62ffc' {
    menuentry 'Debian GNU/Linux, with Linux 4.18.0-0.bpo.1-rt-amd64 (recovery mode)' --class debian --class gnu-linux --class gnu --class os $menuentry_id_option 'gnulinux-4.18.0-0.bpo.1-rt-amd64-recovery-38ea4a12-6cfe-4ed9-a8b5-036295e62ffc' {
    menuentry 'Debian GNU/Linux, with Linux 4.18.0-0.bpo.1-amd64' --class debian --class gnu-linux --class gnu --class os $menuentry_id_option 'gnulinux-4.18.0-0.bpo.1-amd64-advanced-38ea4a12-6cfe-4ed9-a8b5-036295e62ffc' {
    menuentry 'Debian GNU/Linux, with Linux 4.18.0-0.bpo.1-amd64 (recovery mode)' --class debian --class gnu-linux --class gnu --class os $menuentry_id_option 'gnulinux-4.18.0-0.bpo.1-amd64-recovery-38ea4a12-6cfe-4ed9-a8b5-036295e62ffc' {
    menuentry 'Debian GNU/Linux, with Linux 4.17.0-0.bpo.1-amd64' --class debian --class gnu-linux --class gnu --class os $menuentry_id_option 'gnulinux-4.17.0-0.bpo.1-amd64-advanced-38ea4a12-6cfe-4ed9-a8b5-036295e62ffc' {
    menuentry 'Debian GNU/Linux, with Linux 4.17.0-0.bpo.1-amd64 (recovery mode)' --class debian --class gnu-linux --class gnu --class os $menuentry_id_option 'gnulinux-4.17.0-0.bpo.1-amd64-recovery-38ea4a12-6cfe-4ed9-a8b5-036295e62ffc' {
    menuentry 'Debian GNU/Linux, with Linux 4.9.0-8-amd64' --class debian --class gnu-linux --class gnu --class os $menuentry_id_option 'gnulinux-4.9.0-8-amd64-advanced-38ea4a12-6cfe-4ed9-a8b5-036295e62ffc' {
    menuentry 'Debian GNU/Linux, with Linux 4.9.0-8-amd64 (recovery mode)' --class debian --class gnu-linux --class gnu --class os $menuentry_id_option 'gnulinux-4.9.0-8-amd64-recovery-38ea4a12-6cfe-4ed9-a8b5-036295e62ffc' {
```

3) Comment out your current default grub in /etc/default/grub and replace it with the sub-menu's $menuentry_id_option from step one, and the selected kernel's $menuentry_id_option from step two separated by >.

In my case the modified GRUB_DEFAULT is:

``` shell
#GRUB_DEFAULT=0

GRUB_DEFAULT="gnulinux-advanced-38ea4a12-6cfe-4ed9-a8b5-036295e62ffc>gnulinux-4.18.0-0.bpo.1-amd64-advanced-38ea4a12-6cfe-4ed9-a8b5-036295e62ffc"
```

4) Update grub to make the changes. For Debian this is done like so:

``` shell
$ sudo update-grub

$ sudo grub-mkconfig -o /boot/grub/grub.cfg
```
5) Changing this back to the most recent kernel is as simple as commenting out the new line and uncommenting #GRUB_DEFAULT=0:

``` shell
GRUB_DEFAULT=0

#GRUB_DEFAULT="gnulinux-advanced-38ea4a12-6cfe-4ed9-a8b5-036295e62ffc>gnulinux-4.18.0-0.bpo.1-amd64-advanced-38ea4a12-6cfe-4ed9-a8b5-036295e62ffc"
```
then rerunning update-grub.


copy from [Set default kernel in GRUB](https://unix.stackexchange.com/questions/198003/set-default-kernel-in-grub)
also see [How To Set Default Grub / kernel / boot option on Ubuntu GNU/Linux 14.04](http://www.humans-enabled.com/2014/08/how-to-set-default-grub-kernel-boot.html)


## kde

``` shell
sudo pacman -S plasma kde-applications
```

## install i3 desktop

``` shell
sudo pacman -S i3-gaps i3blocks i3lock i3status

sudo pacman -S lightdm lightdm-gtk-greeter
sudo systemctl enable lightdm

sudo pacman -S alacritty
```
copy from [Arch Linux - 安装X Window和i3](https://zhuanlan.zhihu.com/p/384715418)


## xfce

``` shell
sudo pacman -S xfce4 xfce4-goodies
```
copy from [Install XFCE Desktop on Arch Linux](https://linoxide.com/install-xfce-desktop-on-arch-linux/)

also see [Archlinux安装xfce4桌面及美化流程](https://blog.csdn.net/kingolie/article/details/76723448)

## install firefox, google-chrome, tmux
``` shell
sudo pacman -S firefox tmux
yay -S google-chrome
```

## linux-lts
```` shell
sudo pacman -S linux-lts-headers linux-lts linux-lts-docs
```

## efi installation

``` shell

parted /dev/nvme0n1
   mklabel gpt
   mkpart primary 4096s 512M
   mkpart primary 512M  100%
   set 1 boot on
   q

mkfs.vfat -F32 /dev/nvme0n1p1
mkfs.btrfs -f /dev/nvme0n1p2


parted /dev/sda
   mklabel gpt
   mkpart primary 4096s 100%
   q


mkfs.btrfs -f /dev/sda2

// grub-install --target=x86_64-efi --efi-directory=<EFI 分区挂载点> --bootloader-id=arch_grub --recheck
grub-install --target=x86_64-efi --efi-directory=/boot --bootloader-id=arch_grub --recheck

mkdir /boot/EFI/boot
cp boot/EFI/arch_grub/grubx64.efi  /boot/EFI/boot/bootx64.efi
grub-mkconfig -o /boot/grub/grub.cfg
```
copy from [安装archlinux 后，在grub没报错情况下，重启没有grub启动项目](https://bbs.archlinuxcn.org/viewtopic.php?id=2895)
also see [Installation guide](https://wiki.archlinux.org/title/Installation_guide#GRUB_2)

## normal disk partition

``` shell
parted /dev/sda
set 1 boot off
set 1 bios_grub on
q
```
copy from [grub2-install: "this GPT partition label contains no BIOS Boot Partition"](https://superuser.com/questions/903112/grub2-install-this-gpt-partition-label-contains-no-bios-boot-partition)


## fix fireware warning
firmware warning
```
==> WARNING: Possibly missing firmware for module: aic94xx
==> WARNING: Possibly missing firmware for module: bfa
==> WARNING: Possibly missing firmware for module: qed
==> WARNING: Possibly missing firmware for module: qla1280
==> WARNING: Possibly missing firmware for module: qla2xxx
==> WARNING: Possibly missing firmware for module: wd719x
==> WARNING: Possibly missing firmware for module: xhci_pci

```
just run the following command:

``` shell
yay -S wd719x-firmware aic94xx-firmware upd72020x-fw linux-firmware-qlogic
sudo mkinitcpio -p linux
```
see [Arch Linux 更新出现模块固件缺失的警告](https://zhuanlan.zhihu.com/p/340918736)
see [Warning about missing “new” firmware modules](https://forum.endeavouros.com/t/warning-about-missing-new-firmware-modules/23052)

If install some firmware with network error, say upd72020x-fw, you can solve by this:

``` shell
cd ~/.cache/yay/upd72020x-fw
// see the download link
cat PKGBUILD

// for example, wget the pkg
proxychains wget -c https://raw.githubusercontent.com/denisandroid/uPD72020x-Firmware/master/UPDATE.mem
proxychains wget -c https://raw.githubusercontent.com/denisandroid/uPD72020x-Firmware/master/License.rtf
```

## enable multilib

``` shell
sudo vim /etc/pacman.conf
```
add the following into the file:

``` shell
[multilib]
Include = /etc/pacman.d/mirrorlist
```

Upgrade your system:

``` shell
sudo pacman -Syyu
```
Show 32-bit packages in the multilib repository:

``` shell
pacman -Sl | grep -i lib32
```
copy from [Arch Linux How to Enable Multilib](https://low-orbit.net/arch-linux-how-to-enable-multilib)

## swap ctrl and caps

``` shell
sudo mkdir -p /usr/local/share/kbd/keymaps
sudo cp /usr/share/kbd/keymaps/i386/qwerty/us.map.gz /usr/local/share/kbd/keymaps/modified.us.map.gz
```

Change every occurency of Escape to Caps_Lock and every occurency of Caps_Lock to Escape in the copied file and rename it to something like us.map.gz

``` shell
keycode  29 = Caps_Lock
keycode  58 = Control

```

set the config:

``` shell
sudo vim  /etc/vconsole.conf
------------------------------------------
KEYMAP="/usr/local/share/kbd/keymaps/modified.us.map.gz"
```

reboot

copy from [Step by step solution](https://unix.stackexchange.com/questions/616290/how-to-swap-escape-and-caps-lock-for-tty-and-x11)

## using loadkeys swap ctrl and caps

swap-caps-ctrl.map
``` shell
keymaps 0-255
keycode 58 = Control #This makes Caps act as Ctrl
keycode 29 = Caps_Lock #This makes Ctrl act as Caps
alt_is_meta #This fixes the Alt key



# Then run loadkeys on that file:
# $ sudo loadkeys swap-caps-ctrl.map
# To revert, run:
# $ sudo loadkeys -d #load default keymap file

```

run it :

``` shell
sudo loadkeys swap-caps-ctrl.map
```

get the keyboard map:

``` shell
dumpkeys > map
```
copy from [终端模式下交换caps和ctrl键](https://cxymm.net/article/sheismylife/51069348)

## system-timer and service
see [使用Systemd配置定时任务](https://www.jianshu.com/p/4d457985b831)
alas see [Systemd 定时器教程](http://www.ruanyifeng.com/blog/2018/03/systemd-timer.html)

## cannot shutdown customized kernel

``` shell
cat /proc/sys/kernel/printk

echo "7 4 7 4" > /proc/sys/kernel/printk

awk -F: '/:/{print $1}' /proc/net/dev | xargs -i ip link set dev '{}' down
```
see [工作问题案例：设备关机变重启](https://blog.csdn.net/Longyu_wlz/article/details/112122328)
It might because while shutdowning the linux, the network devices are still working.
So set the network device down before shutdowning the computer.

## Systemd Filtering output

``` shell
// Show all messages from this boot:
# journalctl -b

// Include explanations of log messages from the message catalog where available
# journalctl -x

// Show all messages from date (and optional time)
# journalctl --since="2012-10-30 18:17:16"

// Show all messages since 20 minutes ago
# journalctl --since "20 min ago"

// Follow new messages
# journalctl -f

// Show all messages by a specific executable:
# journalctl /usr/lib/systemd/systemd

// Show all messages by a specific process
# journalctl _PID=1

// Show all messages by a specific unit
# journalctl -u man-db.service

// Show all messages from user services by a specific unit
$ journalctl --user -u dbus

// Show kernel ring buffer
# journalctl -k

// Show only error, critical and alert priority messages
# journalctl -p err..alert

// Show auth.log equivalent by filtering on syslog facility
# journalctl SYSLOG_FACILITY=10

// It can be sped up significantly by using --file option to force journalctl to look only into most recent journal
# journalctl --file /var/log/journal/*/system.journal -f
```
copy from [Systemd](https://wiki.archlinux.org/title/Systemd/Journal#Filtering_output)


## update system gpg

``` shell
sudo rm -fr /etc/pacman.d/gnupg
sudo pacman-key --init
sudo pacman-key --populate archlinux
sudo pacman -S archlinux-keyring
sudo pacman-key --refresh-keys
sudo pacman -Syyu
```
copy from [upgrade FAILS for one missing key](https://archived.forum.manjaro.org/t/upgrade-fails-for-one-missing-key/154988)

## timeshift

``` shell
sudo pacman -S timeshift

sudo timeshift --list
sudo timeshift --snapshot-device /dev/sdb4
sudo timeshift --restore --snapshot '2019-07-16_16-35-42' --skip-grub

// --tags D stands for Daily Backup
// --tags W stands for Weekly Backup
// --tags M stands for Monthly Backup
// --tags O stands for On-demand Backup
sudo timeshift --create --comments "A new backup" --tags D

sudo timeshift --restore

// more usage on timeshift
timeshift --help
```
copy from [Archlinux 优化之一](https://blog.tiantian.cool/arch-1/)

## makepkg optimization

``` shell
vim ~/.makepkg.conf
# -------------------------------------------
CFLAGS="-march=native -O2 -pipe -fno-plt"
CXXFLAGS="-march=native -O2 -pipe -fno-plt"

MAKEFLAGS="-j$(nproc)"

BUILDENV=(!distcc color ccache !check !sign)
BUILDDIR=/tmp/makepkg

COMPRESSXZ=(xz -c -z - --threads=0)
```
copy from [Archlinux 优化之一](https://blog.tiantian.cool/arch-1/)

## podman

``` shell
sudo pacman -S podman

echo "$USER:110000:65536" | sudo tee -a  /etc/subuid
echo "$USER:110000:65536" | sudo tee -a  /etc/subgid

podman system migrate

mkdir -p $HOME/.config/containers/
echo -e "[registries.search]\nregistries = ['docker.io']" | tee $HOME/.config/containers/registries.conf
```
podman usage is just the same with docker.
docker is no more needed.

## fuse-overlayfs

``` shell
sudo pacman -S fuse-overlayfs

podman --storage-opt mount_program=/usr/bin/fuse-overlayfs --storage-opt ignore_chown_errors=true run [...]
```
copy from [Error: kernel does not support overlay fs: 'overlay' is not supported over extfs](https://unix.stackexchange.com/questions/689181/error-kernel-does-not-support-overlay-fs-overlay-is-not-supported-over-extfs)

## libvoikko

``` shell
sudo pacman -S libvoikko
```

## genymotion -- android emulator

``` shell
sudo pacman -S genymotion
```

## xdroid-bin -- android emulator

``` shell
yay -S xdroid-bin
```

## libpam-google-authenticator

``` shell
sudo pacman -S libpam-google-authenticator
```

copy from [为Linux系统开启多因素认证](http://blog.lujun9972.win/blog/2020/08/15/%E4%B8%BAlinux%E7%B3%BB%E7%BB%9F%E5%BC%80%E5%90%AF%E5%A4%9A%E5%9B%A0%E7%B4%A0%E8%AE%A4%E8%AF%81/index.html)

## ata1: softreset failed (device not ready)

kernel option
``` shell
libata.force=norst
```

copy from [CONFIG_SATA_PMP=n, HW bug, Live Arch Debian Mint does not work.](https://www.linuxquestions.org/questions/linux-newbie-8/config_sata_pmp%3Dn-hw-bug-live-arch-debian-mint-does-not-work-4175549085/)

## GDM ignores Wayland and uses X.Org by default

``` shell
/etc/mkinitcpio.conf
MODULES=(nvidia)
```
copy from [GDM ignores Wayland and uses X.Org by default](https://wiki.archlinux.org/title/GDM#GDM_ignores_Wayland_and_uses_X.Org_by_default)
also see [Kernel mode setting](https://wiki.archlinux.org/title/Kernel_mode_setting#Early_KMS_start)

## sysrq
see [linux下的SysRq键](http://blog.lujun9972.win/blog/2018/08/22/linux%E4%B8%8B%E7%9A%84sysrq%E9%94%AE/index.html)

## get syslog

``` shell
dmesg --level=alert,crit,err
journalctl | grep -i "error\|warn\|fail\|acpi"
journalctl -b 1
```

## perl-rename

``` shell
sudo pacman -S perl-file-rename
echo 'alias rename=perl-rename' | tee -a ~/.zshrc
```

## telnet

``` shell
pacman -S inetutils xinetd
```

## tigervnc

``` shell
$ sudo pacman -S tigervnc

$ sudo vim /etc/tigervnc/vncserver.users
------------------
:1={USER_NAME}  ## replace your user name here

$ vncpasswd

$ cat ~/.vnc/config
session=gnome
geometry=1920x1080
localhost
alwaysshared

$ sudo systemctl start vncserver@:1.service
$ sudo systemctl enable vncserver@:1.service
```
then use ssh:

``` shell
vim ~/.ssh/config

Host archlinux
    HostName 10.0.0.1 # use your ip here
    User gerald
    LocalForward 5901 localhost:5901
```

then run the command:

``` shell
// type your password and login
ssh archlinux

// use the vncviewer command
vncviewer localhost:5901
```

copy from [TigerVNC](https://wiki.archlinux.org/title/TigerVNC)
also see [archlinux安装配置vnc+openbox](https://cxybb.com/article/lxyoucan/116780297)
also see [How to Install TightVNC to Access Remote Desktops in Linux](https://www.tecmint.com/install-tightvnc-access-remote-desktop-in-linux/)

## disable sleep

``` shell
sudo systemctl mask sleep.target suspend.target hibernate.target hybrid-sleep.target
```
copy from [禁止ubuntu 20.04自动休眠](https://zhuanlan.zhihu.com/p/415661679)

## filename higher than 255 characters

``` shell
/usr/src/linux-headers-2.6.38-10/include/linux/limits.h

 #define NAME_MAX         255    /* # chars in a file name */
 #define PATH_MAX        4096    /* # chars in a path name including nul */
```
copy from [Filename length limits on linux?](https://serverfault.com/questions/9546/filename-length-limits-on-linux)

## disable gnome-keyring ssh integration

``` shell
(cat /etc/xdg/autostart/gnome-keyring-ssh.desktop; echo Hidden=true) > ~/.config/autostart/gnome-keyring-ssh.desktop
```
copy from [How do I disable gnome-keyring ssh integration?](https://askubuntu.com/questions/545172/how-do-i-disable-gnome-keyring-ssh-integration)



## mount disk in /etc/fstab

```
#UUID=ebb41841-85fd-4d22-9f33-c88348ff18c4   /      	btrfs deafults 0 0
#/dev/sda1   /      	btrfs      	defaults  	0 0
PARTUUID=ebb41841-85fd-4d22-9f33-c88348ff18c4   /      	btrfs      	defaults  	0 0
```

## change nic interface name

``` shell
cat /sys/class/net/enp0s4/addres

sudo vim /etc/udev/rules.d/10-network.rules
--------------------------------------------
SUBSYSTEM=="net", ACTION=="add", ATTR{address}=="aa:bb:cc:dd:ee:ff", NAME="net1"
SUBSYSTEM=="net", ACTION=="add", ATTR{address}=="ff:ee:dd:cc:bb:aa", NAME="net0"

```
copy from [Change interface name](https://wiki.archlinux.org/title/Network_configuration#Change_interface_name)


## android filesystem support

``` shell
sudo pacman -S mtpfs
yay -S jmtpfs

sudo chmod 777 /mnt                      //this is very import

jmtpfs /mnt                             //to mount android to /mnt

fusermount -u /mnt                      //umount mnt

```
copy from [How to Transfer Files from Arch Linux to Android](https://www.jianshu.com/p/e90f9e45fe62)

## at batch command line

``` shell
sudo pacman -S at
sudo systemctl enable --now atd
at 09:00 -f /home/linuxize/script.sh

at 09:00 <<END
command_to_be_run
END

atq
// get the run command with the job id number, 3 is in the output list of the atq
at -c 3
atrm 1
```

## lldb-mi

``` shell
yay -S lldb-mi-git
```

## texlive and pandoc

``` shell
// sudo vim /etc/pacman.conf
-----------------
// [Clansty]
// SigLevel = Never
// Server = https://repo.lwqwq.com/archlinux/$arch
// Server = https://pacman.ltd/archlinux/$arch
// Server = https://repo.clansty.com/archlinux/$arch

// sudo pacman -Suw texlive-full
// sudo pacman -S texlive-full

// wget -c https://pacman.ltd/archlinux/pool/texlive-full-20220220-1-x86_64.pkg.tar.zst
// sudo pacman -U texlive-full-20220220-1-x86_64.pkg.tar.zst

sudo pacman -S pandoc texlive-core texlive-latexextra texlive-fontsextra texlive-langchinese texlive-most texlive-lang texlive-pstricks
yay -S ttf-sarasa-ui-sc
pandoc README.md -o README.pdf --pdf-engine=xelatex -V CJKmainfont='Sarasa UI SC'
pandoc README.md -o README.docx

```
copy from [ArchLinux 安装 TeXLive](https://zhuanlan.zhihu.com/p/417566961)

## appflowy

``` shell
yay -S appflowy-bin
```

##  wireshark-cli

``` shell
sudo pacman -S wireshark-cli
```

## minio

``` shell
sudo pacman -S minio minio-client
```

## wine

``` shell
sudo pacman -S wine-mono wine
```

## cronie

``` shell
sudo pacman -S cronie
sudo systemctl enable --now cronie.service
EDITOR="emacs -nw" crontab -e
```

## alsamixer

``` shell
sudo pacman -S alsa-utils
alsamixer
// save the setting
sudo alsactl store
```

## podman start error

``` shell
$ podman run -it --rm --entrypoint=/bin/bash --privileged --ulimit nofile=262144:262144 -v $PWD:/work -p 9000:9000 -p 18123:8123 yandex/clickhouse-server:21.3.20.1
Error: crun: error stat'ing file `/dev/vboxusb/001/007`: Permission denied: OCI permission denied
```
it might be error to be fixed by：
[Privileged containers cannot be restarted if host devices changed](https://github.com/containers/podman/issues/13899)

## docker

``` shell
sudo pacman -S docker
sudo usermod -aG docker $USER
sudo systemctl enable --now docker
```

## lld

``` shell
sudo pacman -S lld
```
copy from [Cargo build failed with = note: collect2: fatal error: cannot find 'ld'](https://stackoverflow.com/questions/70272393/cargo-build-failed-with-note-collect2-fatal-error-cannot-find-ld)

## zram-generator

``` shell
sudo pacman -S zram-generator

echo "[zram0]
zram-size = ram / 2" | sudo tee -a /etc/systemd/zram-generator.conf

sudo systemctl daemon-reload

sudo systemctl start /dev/zram0

zramctl
```
swap is no more needed.
copy from [systemctl start /dev/zram0](https://github.com/systemd/zram-generator)
The reason why choose zram:
1, the kubernetes dislike swap
2, the zram is much more high performance.

Or change the zram-generator.conf:

```
[zram0]
zram-fraction = 1
max-zram-size=none
```
copy from [Dynamically Increase SWAP (ZRAM) Size in Linux](https://medium.com/nerd-for-tech/dynamically-adjust-swap-zram-size-in-fedora-linux-78cd712808f2)

kubernetes does not accept swap, the zram is also not accepted.
Do not install zram.

## visual-studio-code-bin
for unreal engine
``` shell
yay -S visual-studio-code-bin
```


## install flutter

``` shell
sudo pacman -S flutter
sudo usermod -aG flutterusers $USER
```

## /usr/bin/dkms: line 1033: sha512: command not found

``` shell
sudo ln -s /usr/bin/sha512sum /usr/bin/sha512
```
copy from [/usr/sbin/dkms: line 1033: sha512: command not found.](https://bbs.archlinux.org/viewtopic.php?id=277700)
also see [https://bbs.archlinux.org/viewtopic.php?id=277700](https://github.com/dell/dkms/issues/229)


## cpupower

``` shell
sudo pacman -S cpupower

cpupower frequency-info

cat /etc/default/cpupower
min_freq="1.20GHz"
max_freq="2.40GHz"

sudo systemctl restart cpupower
sudo systemctl start cpupower
```

## systemd-networkd-wait-online.service fail

``` shell
[Service]
ExecStart=
ExecStart=/usr/lib/systemd/systemd-networkd-wait-online --ignore=enp0s31f6
```

copy from [systemd-networkd-wait-online failure](https://unix.stackexchange.com/questions/381448/systemd-networkd-wait-online-failure)

## qemu

``` shell
sudo pacman -Syu libvirt qemu ebtables dnsmasq
sudo systemctl enable libvirtd.service --now
sudo systemctl enable virtlogd.service --now
```
copy from [Kubernetes on your local Manjaro/Arch](https://medium.com/@morgan_42683/kubernetes-on-your-local-manjaro-arch-7890904c8984)

## wpa_supplicant

wpa_supplicant.service:

``` shell
sudo vim /lib/systemd/system/wpa_supplicant.service
```
Find the following line.

``` shell
ExecStart=/sbin/wpa_supplicant -u -s -O /run/wpa_supplicant
```
change to be:

``` shell
ExecStart=/sbin/wpa_supplicant -u -s -c /etc/wpa_supplicant/wpa_supplicant.conf -i wlp0s20u9
Restart=always
```
then start the service

``` shell
sudo systemctl enable wpa_supplicant.service
sudo systemctl start wpa_supplicant.service
```
