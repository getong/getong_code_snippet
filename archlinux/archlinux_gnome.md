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
