#!/bin/sh

## EFI install archlinux

reflector --country China --age 72 --sort rate --protocol https --save /etc/pacman.d/mirrorlist

timedatectl set-ntp true

## parted ssd disk
parted /dev/nvme0n1 – mklabel gpt
parted /dev/nvme0n1 - mklabel gpt
parted /dev/nvme0n1 - mkpart primary 2M 512M
parted /dev/nvme0n1 - pmkpart primary 512M   -1
parted /dev/nvme0n1 - set 1 boot on

mkfs.vfat -F32 /dev/nvme0n1p1
mkfs.btrfs -f /dev/nvme0n1p2

## parted normal disk
parted /dev/sda - mklabel gpt
parted /dev/sda - mkpart primary 2M 128G
parted /dev/sda - mkpart primary 128G -1

mkswap /dev/sda1
mkfs.btrfs -f /dev/sda2

## mount disk
mount /dev/nvme0n1p2 /mnt

mkdir /mnt/boot
mount /dev/nvme0n1p1 /mnt/boot

mkdir /mnt/backup
mount /dev/sda2 /mnt/backup

pacstrap /mnt linux linux-firmware linux-headers base base-devel vim git \
    net-tools openssh gdm xorg xorg-server xorg-xinit xorg-xrandr \
    gnome gnome-extra gnome-tweak-tool gnome-shell grub efibootmgr \
    intel-ucode proxychains v2ray asp nemo emacs julia erlang \
    gnome-software-packagekit-plugin gnome-tweaks pacman-contrib \
    util-linux vagrant w3m wget xf86-video-nouveau xf86-video-intel mesa-libgl \
    wqy-zenhei cmake reflector bc nano zsh trash-cli vlc cpio xmlto python-sphinx_rtd_theme \
    virtualbox virtualbox-host-dkms virtualbox-guest-iso virtualbox-ext-oracle \
    adobe-source-han-serif-cn-fonts noto-fonts-cjk noto-fonts-emoji noto-fonts-extra \
    adobe-source-han-sans-cn-fonts ttf-sarasa-gothic texlive-core texlive-latexextra \
    nfs-utils mpv wpa_supplicant samba pandoc texlive-fontsextra texlive-langchinese \
    gst-libav a52dec faac faad2 flac jasper lame libdca libdv libmad libmpeg2 \
    libtheora libvorbis libxv wavpack x264 xvidcore gstreamer0.10-base-plugins \
    fcitx-configtool fcitx fcitx-gtk3 sunpinyin fcitx-sunpinyin calibre gthumb \
    netease-cloud-music moc authy vdhcoapp feishu-bin firefox firefox-i18n-zh-cn

genfstab -U /mnt >> /mnt/etc/fstab

arch-chroot /mnt
