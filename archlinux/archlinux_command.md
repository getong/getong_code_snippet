# archlinux command

## set the mirror and update

``` shell
cd /etc/pacman.d/
sudo mv mirrorlist  mirrorlist.bak
echo "Server = http://mirrors.163.com/archlinux/$repo/os/$arch" | sudo tee  mirrorlist
sudo pacman -Syu
```
