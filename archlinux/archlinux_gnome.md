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
sudo pacman -S a52dec faac faad2 flac jasper lame libdca libdv libmad libmpeg2 libtheora libvorbis libxv wavpack x264 xvidcore gstreamer0.10-base-plugins
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
revert the change:

``` shell
gsettings set org.gnome.desktop.input-sources xkb-options "[]"
```

use with localectl:

``` shell
localectl set-x11-keymap "" "" "" ctrl:swapcaps

// or
localectl --no-convert set-x11-keymap us pc105 "" ctrl:swapcaps
```
revert the change:

``` shell
 localectl set-x11-keymap "" "" "" ""
```

check the settings:

``` shell
$ localectl status
   System Locale: LANG=zh_CN.UTF-8
       VC Keymap: n/a
      X11 Layout: n/a
     X11 Options: ctrl:swapcaps

```



## install fcitx

``` shell
sudo pacman -S fcitx-configtool fcitx fcitx-gtk3 sunpinyin fcitx-sunpinyin

sed -i 's/TriggerKey.*$/TriggerKey=CTRL_SHIFT_LCTRL CTRL_SHIFT_LSHIFT/g' ~/.config/fcitx/config

gsettings set org.gnome.settings-daemon.plugins.xsettings overrides "{'Gtk/IMModule':<'fcitx'>}"
```

or add the adove into `/etc/environment` file:

``` shell
GTK_IM_MODULE=fcitx
QT_IM_MODULE=fcitx
XMODIFIERS="@im=fcitx"
INPUT_METHOD="fcitx"
QT4_IM_MODULE="fcitx"
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
gsettings set org.gnome.desktop.screensaver lock-delay 0
gsettings set org.gnome.desktop.session idle-delay 300

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

also set by command:

``` shell
sudo -u gdm dbus-launch gsettings set org.gnome.settings-daemon.plugins.power sleep-inactive-ac-type 'nothing'
```
also
``` shell
You can disable (or configure) auto-suspend system-wide by creating a file /etc/dconf/db/local.d/00-autosuspend with this or similar contents:

[org/gnome/settings-daemon/plugins/power]
# Do not autosuspend
sleep-inactive-ac-type='nothing'
sleep-inactive-battery-type='nothing'

and running:

sudo dconf update
```
copy from [Auto-suspending despite of settings to the contrary after update](https://bbs.archlinux.org/viewtopic.php?id=236180)

or:

``` shell
$ IFS=$'\n'; for x in $(sudo -u YOUR_USER gsettings list-recursively org.gnome.settings-daemon.plugins.power); do eval "sudo -u gdm dbus-launch gsettings set $x"; done; unset IFS
```
copy from [Computer suspends when not logged in](https://superuser.com/questions/1309219/computer-suspends-when-not-logged-in)

## weixin
enable multilib first
``` shell
yay -S deepin-wine-wechat com.qq.weixin.work.deepin deepin-wine-qq
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
install ttf-dejavu and ttf-droid
``` shell
sudo pacman -S ttf-dejavu ttf-droid
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

## translations for nemo

``` shell
yay -S cinnamon-translations
```

## install vulkan
check the video card brand:

``` shell
lspci -vnn | grep VGA -A 12

// or use this:
sudo lshw -C display

// check the video card driver:
sudo lshw -c video | grep configuration

// get driver info
modinfo i915

// get the hardware speedup
glxinfo | grep OpenGL
```
copy from [Linux中如何查看显卡硬件信息](https://www.ancii.com/aqmpx33bj/)

## install vulkan

Intel:

``` shell
sudo pacman -S vulkan-intel vulkan-icd-loader
```

NVIDIA:

``` shell
sudo pacman -S nvidia-utils vulkan-icd-loader
```

AMD:
``` shell
sudo pacman -S amdvlk vulkan-icd-loader
```
copy from [Vulkan](https://wiki.archlinux.org/title/Vulkan)

## instant message software
yay use http_proxy and https_proxy environment variable
``` shell
yay -S slack-desktop zulip-desktop-bin skypeforlinux-stable-bin
```


## stop skype auto start


``` shell
cat ~/.config/autostart/skypeforlinux.desktop
[Desktop Entry]
Name=Skype for Linux
Comment=Skype Internet Telephony
Exec=/usr/bin/skypeforlinux
Icon=skypeforlinux
Terminal=false
Type=Application
StartupNotify=false
X-GNOME-Autostart-enabled=true%

mv ~/.config/autostart/skypeforlinux.desktop ~/.config/autostart/skypeforlinux.desktop.bak
```

## pdf reader

``` shell
yay -S foxitreader
```
## calibre

``` shell
sudo pacman -S calibre
```

## aliyunpan

``` shell
yay -S pyinstaller aliyunpan-cli-bin
```

## arc dark theme

``` shell
sudo pacman -S arc-gtk-theme
```
copy from [Install Arc And Arc Darker Theme](https://www.addictivetips.com/ubuntu-linux-tips/install-the-arc-and-arc-darker-theme-linux/)

activate the arc theme:

```
GNOME: GNOME Tweak Tool -> Appearance -> choose “Arc” or other flavors.
```
copy from [Install Arc GTK Theme on Ubuntu](https://linuxhint.com/install_arc_gtk_theme_ubuntu/)

## switch to iwd
see [wpa_supplicant shutdown job never ends, need force power down](https://bbs.archlinux.org/viewtopic.php?id=246942)
>>>
While the original issue was not solved, I marked this thread as solved:
I switched from wpa_supplicant to iwd and the problem is gone now.
My laptop shuts down almost immediately now and my Wifi is working very good all the time!

connect to wifi device:
``` shell
sudo pacman -S iwd
sudo systemctl enable --now iwd
sudo systemctl start iwd

iwctl adapter list
iwctl device list

// get the list of the available connections
iwctl station wlan0 get-networks
// To verify the connection is now active
iwctl station wlan0 show

// disconnecting from a network
iwctl station wlan0 disconnect

// Obtaining a list of the known connections
iwctl known-networks list

// To make the service forget about the “arda” network
iwctl known-networks arda forget

iwctl --passphrase passphrase station <device> connect SSID
// or
iwctl --passphrase <passphrase> station <device> connect-hidden <ssid>
```

static the ip address:

``` shell
/var/lib/iwd/spaceship.psk
[IPv4]
Address=192.168.1.80
Netmask=255.255.255.0
Gateway=192.168.1.1
Broadcast=192.168.1.255
DNS=192.168.1.253

[Settings]
AutoConnect=true
```

use systemd-resolved

``` shell
sudo vim /etc/iwd/main.conf
-------------------------------
[Network]
NameResolvingService=systemd
EnableIPv6=true

[General]
use_default_interface=true
EnableNetworkConfiguration=true
route_priority_offset=100

[Scan]
DisablePeriodicScan=true
```

see [Connecting to a hidden Wi-Fi network Arch Linux](https://unix.stackexchange.com/questions/664646/connecting-to-a-hidden-wi-fi-network-arch-linux)
also see [simple wifi setup with iwd and networkd](https://insanity.industries/post/simple-wifi/)
also see [iwd (简体中文)](https://wiki.archlinux.org/title/Iwd_(%E7%AE%80%E4%BD%93%E4%B8%AD%E6%96%87))
also see [How to manage wireless connections using iwd on Linux](https://linuxconfig.org/how-to-manage-wireless-connections-using-iwd-on-linux)
also see [Switching from netctl to networkd with iwd](https://www.rdeeson.com/weblog/180/switching-from-netctl-to-networkd-with-iwd)
also see [iwd](https://wiki.archlinux.org/title/Iwd)

## gthumb

``` shell
sudo pacman -S gthumb
```

## bluetooth

``` shell
sudo pacman -S bluez bluez-utils bluez-tools
sudo systemctl enable --now bluetooth.service

rfkill block bluetooth
rfkill unblock bluetooth

bluetoothctl
> help
> power on
> devices
> scan on
> pair $MAC
> connect $MAC

vim /etc/bluetooth/main.conf
AutoEnable=true
```

copy from [bluetooth](https://wiki.archlinux.org/title/bluetooth)

## music

``` shell
sudo pacman -S netease-cloud-music moc
yay -S qqmusic-bin
```

## baidunetdisk-electron

``` shell
yay -S baidunetdisk-electron
```

## disable sound

``` shell
sudo -u gdm dbus-launch gsettings set org.gnome.desktop.sound event-sounds 'false'
```
copy from [GDM](https://wiki.archlinux.org/title/GDM)

## authy

``` shell
sudo pacman -S authy
```

## video download helper

``` shell
sudo pacman -S vdhcoapp
```

## fix Authentication is required to create a color managed device

``` shell
sudo groupadd vnc
sudo usermod -aG vnc $USER

sudo vim /etc/polkit-1/rules.d/gnome-vnc.rules
---------------------------
   polkit.addRule(function(action, subject) {
      if ((action.id == "org.freedesktop.color-manager.create-device" ||
           action.id == "org.freedesktop.color-manager.create-profile" ||
           action.id == "org.freedesktop.color-manager.delete-device" ||
           action.id == "org.freedesktop.color-manager.delete-profile" ||
           action.id == "org.freedesktop.color-manager.modify-device" ||
           action.id == "org.freedesktop.color-manager.modify-profile") &&
          subject.isInGroup("vnc")) {
         return polkit.Result.YES;
      }
   });


sudo systemctl restart vncserver@:1.service
```
copy from [启动 GNOME 3 时显示 "Authentication is required to create a color managed device" 对话框](https://wiki.archlinux.org/title/TigerVNC_(%E7%AE%80%E4%BD%93%E4%B8%AD%E6%96%87))

## launch application in remote desktop via ssh

``` shell
export DISPLAY=:0
gedit
```
copy from [Start Gnome-Application from SSH-Shell](https://stackoverflow.com/questions/3664112/start-gnome-application-from-ssh-shell)

The vnc desktop is :1 , so the command can be changed to:

``` shell
export DISPLAY=:1
gedit
```

## feishu

``` shell
sudo pacman -S feishu-bin
```

## nixnote2

``` shell
sudo pacman -S nixnote2
```
copy from [Archlinux安装印象笔记](https://www.cnblogs.com/mc-r/p/13762982.html)

## ynote-desktop-bin

``` shell
yay -S ynote-desktop-bin
```
