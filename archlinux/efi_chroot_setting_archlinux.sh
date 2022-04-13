#!/bin/sh

ln -sf /usr/share/zoneinfo/Asia/Shanghai /etc/localtime
hwclock --systohc

# locale
sed --in-place=.bak 's/^#en_US\.UTF-8/en_US\.UTF-8/' /etc/locale.gen
sed --in-place=.bak2 's/^#zh_CN\.UTF-8/zh_CN\.UTF-8/' /etc/locale.gen
locale-gen
echo "LANG=zh_CN.UTF-8" > /etc/locale.conf

echo archlinux > /etc/hostname

## copy from [装Arch记](http://blog.lujun9972.win/blog/2016/10/20/%E8%A3%85arch%E8%AE%B0/)
cat >>/etc/pacman.conf <<EOF
[archlinuxcn]
Server = https://mirrors.ustc.edu.cn/archlinuxcn/\$arch
EOF

pacman -Syyu
pacman --noconfirm -S archlinuxcn-keyring

pacman --noconfirm -S yay

systemctl enable sshd.service

systemctl enable systemd-networkd.service

systemctl enable systemd-resolved.service

systemctl enable gdm.service

## wps google-chrome
yay --noconfirm -S wps-office-cn ttf-wps-fonts ttf-ms-fonts wps-office-fonts wps-office-mime-cn wps-office-mui-zh-cn google-chrome

echo >> /etc/environment <<EOF
GTK_IM_MODULE=fcitx
QT_IM_MODULE=fcitx
XMODIFIERS="@im=fcitx"
INPUT_METHOD="fcitx"
QT4_IM_MODULE="fcitx"
EOF


echo "Setting root password"
passwd

read -r -p "Enter a new user name:" username
useradd -m -G wheel -s /usr/bin/zsh $username
passwd $username

echo setting up system...

gtk-query-immodules-2.0 | tee -a /etc/gtk-2.0/gtk.immodules

## copy from [How does bash deal with nested quotes?](https://stackoverflow.com/questions/25941394/how-does-bash-deal-with-nested-quotes)
runuser -l $username -c $'gsettings set org.gnome.desktop.input-sources xkb-options "[\'ctrl:swapcaps\']"'
