# archlinux gnome

also see [GNOME/Tips and tricks](https://wiki.archlinux.org/title/GNOME/Tips_and_tricks)

## clock with seconds

``` shell
gsettings set org.gnome.desktop.interface clock-show-seconds false
```

copy from [Ubuntu 18.04 gnome-shell high CPU usage](https://askubuntu.com/questions/1036441/ubuntu-18-04-gnome-shell-high-cpu-usage)

## install gst-libav

``` shell
sudo pacman -S gst-libav
```

## haveged

``` shell
sudo pacman -S haveged
sudo systemctl start haveged
sudo systemctl enable haveged
```

copy from [gdm takes a very long time to load](https://bbs.archlinux.org/viewtopic.php?id=250490)

## gstreamer0.10-plugins

``` shell
sudo pacman -S a52dec faac faad2 flac jasper lame libdca libdv libmad libmpeg2 libtheora libvorbis libxv wavpack x264 xvidcore gstreamer0.10-plugins
```
copy from [7 Essential Things To Do After Installing Arch Linux](https://itsfoss.com/things-to-do-after-installing-arch-linux/)


## very old graphical card

``` shell
lspci -k | grep -A 2 -E "(VGA|3D)"

sudo pacman -S bumblebee

yay -S nvidia-340xx-dkms
sudo cp /usr/share/nvidia-340xx/20-nvidia.conf /etc/X11/xorg.conf.d/
sudo reboot
```
see [NVIDIA (简体中文)](https://wiki.archlinux.org/title/NVIDIA_(%E7%AE%80%E4%BD%93%E4%B8%AD%E6%96%87))

also see [Driver Nvidia legacy no Debian Buster](https://linuxdicasesuporte.blogspot.com/2020/03/driver-nvidia-legacy-no-debian-buster.html)
>>> You must tell Xorg to use the nvidia driver with kernels >=5.11.0.
    You must also set IgnoreABI option with Xorg version >= 21.1.1.
    Minimal config example provided in /usr/share/nvidia-340xx/20-nvidia.conf
    which you should manually place in /etc/X11/xorg.conf.d/

## wps
``` shell
yay -S wps-office-cn ttf-wps-fonts ttf-ms-fonts wps-office-fonts wps-office-mime-cn wps-office-mui-zh-cn
```

## swap ctrl and caps
``` shell
gsettings set org.gnome.desktop.input-sources xkb-options "['ctrl:swapcaps']"
```

## install fcitx

``` shell
sudo pacman -S fcitx-configtool fcitx fcitx-gtk3

sed -i 's/TriggerKey.*$/TriggerKey=CTRL_SHIFT_LCTRL CTRL_SHIFT_LSHIFT/g' ~/.config/fcitx/config

gsettings set org.gnome.settings-daemon.plugins.xsettings overrides "{'Gtk/IMModule':<'fcitx'>}"
```

or add the adove into `/etc/environment` file:

``` shell
export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
export XMODIFIERS="@im=fcitx"
export INPUT_METHOD="fcitx"
export QT4_IM_MODULE="fcitx"
```
see [ArchLinux下fcitx ctrl+space无法调出输入法](https://its201.com/article/r8l8q8/73431256)

then run `fcitx-configtool` to configure fcitx:
``` shell
$ fcitx-configtool
# 在input method那里点加号, 添加Pinyin
# 在global-config进行全局配置
# 在Apperance进行字体大小调整和状态显示
# Addon进行插件管理, 双击插件进行设置
```

## debug fcitx

``` shell
fcitx-diagnose

gtk-query-immodules-2.0 | sudo tee -a /etc/gtk-2.0/gtk.immodules
```
copy from [firefox和chrome无法调出fcitx输入中文](https://groups.google.com/g/archlinux-cn/c/nQUuGCDwL64)


## sound card driver

``` shell
sudo gpasswd -a username video

```
Then add the following to `~/.config/pulse/default.pa`

```
set-card-profile 0 output:analog-stereo+input:analog-stereo
set-sink-port 2 analog-output-lineout
```

## gnome power setting

``` shell
gsettings set org.gnome.settings-daemon.plugins.power power-button-action nothing
gsettings set org.gnome.settings-daemon.plugins.power sleep-inactive-ac-type nothing
gsettings set org.gnome.settings-daemon.plugins.power sleep-inactive-battery-type nothing

// lock screen after 300 seconds idle
gsettings set org.gnome.desktop.screensaver lock-enabled true
gsettings set org.gnome.desktop.screensaver lock-dalay 0
gsettings set org.gnome.desktop.session idle-delay uint32 300

// List available schemas and look for ones related to the power subsystem.
gsettings list-schemas | grep power

// Alternatively, list schemas, keys, and values to look for power-related ones.
gsettings list-recursively | grep "\\.power"

// List keys in a specific schema.
gsettings list-keys org.gnome.settings-daemon.plugins.power

// List keys and values in a specific schema.
gsettings list-recursively org.gnome.settings-daemon.plugins.power

// Get a description of a specific key.
gsettings describe org.gnome.settings-daemon.plugins.power sleep-inactive-ac-timeout

// Get the possible values for a specific key.
gsettings range org.gnome.settings-daemon.plugins.power sleep-inactive-ac-timeout

// Check if a specific key is writable.
gsettings writable org.gnome.settings-daemon.plugins.power sleep-inactive-ac-timeout

// Alter the value of a specific key.
gsettings set org.gnome.settings-daemon.plugins.power sleep-inactive-ac-timeout 3600

// Read the value of a specific key.
gsettings get org.gnome.settings-daemon.plugins.power sleep-inactive-ac-timeout
```

see [How to alter Ubuntu desktop configuration using terminal](https://sleeplessbeastie.eu/2020/08/19/how-to-alter-ubuntu-desktop-configuration-using-terminal/)

## weixin
enable multilib first
``` shell
yay -S com.qq.weixin.deepin com.qq.weixin.work.deepin com.qq.im.deepin
```
config the display effect:

``` shell
WINEPREFIX=~/.deepinwine/Deepin-WeChat deepin-wine6-stable winecfg

// http://39.99.136.201:8088/share/xHYZb0Z5
wget -c http://39.99.136.201:8088/api/public/dl/xHYZb0Z5

sudo tar xzf lib32.tgz -C /opt/apps/com.qq.weixin.work.deepin/files
// 修改run.sh   26行左右   添加 export LD_LIBRARY_PATH=/opt/apps/$DEB_PACKAGE_NAME/files/lib32
```
copy from [Archlinux有没有办法安装企业微信](https://bbs.archlinuxcn.org/viewtopic.php?id=12056)


## install fonts

``` shell
git clone https://github.com/gasharper/linux-fonts
cd linux-fonts
sudo sh install.sh
```
the code in `install.sh` :

``` shell
#!/bin/sh
sudo mkdir /usr/share/fonts/linux_fonts
sudo cp ./*.ttf /usr/share/fonts/linux_fonts
sudo cp ./*.ttc /usr/share/fonts/linux_fonts
cd /usr/share/fonts/linux_fonts
sudo mkfontscale
sudo mkfontdir
sudo fc-cache
sudo chmod 644 /usr/share/fonts/linux_fonts/*
```

check the font:

``` shell
fc-list :lang=zh-cn | sort
```

## fcitx5 can not handle ctrl swapcaps
With fcitx5 chinese input method,  ctrl and caps both act as caps.
Switch back to fcitx.
