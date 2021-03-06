# buster amd64 installation

## upgrade the system

``` shell
# apt-get update
# apt-get upgrade -y
# apt-get install -y vim zsh curl nemo git moc moc-ffmpeg-plugin unzip p7zip-full w3m w3m-img proxychains
# systemctl set-default multi-user.target
```
## add the /sbin to the root user PATH variable

``` shell
export PATH=/sbin:$PATH
```

## compile the linux kernel

``` shell
# apt-get build-dep -y linux-source
# apt-get install -y firmware-linux firmware-linux-nonfree firmware-linux-free firmware-realtek libncurses6 libncurses-dev linux-source build-essential linux-headers-amd64 libelf-dev libssl-dev
# cd /usr/src
# xz -d -k linux-patch-4.19-rt.patch.xz
# tar xaf linux-source-4.19.tar.xz
# cd linux-source-4.19
# patch -p1 < ../linux-patch-4.19-rt.patch
# cp /boot/config-4.19.0-8-amd64 .config
```
edit the .config file

```
CONFIG_SATA_PMP=n
#CONFIG_SYSTEM_TRUSTED_KEYS="debian/certs/debian-uefi-certs.pem"
```
and then compile and install the kernel

``` shell
# make menuconfig
# make -j`nproc` bzImage
# make -j`nproc` modules
# make modules_install
# make headers_install
# make install
# /sbin/shutdown -h now
```

## update-grub
edit the `/etc/default/grub`

```
GRUB_TIMEOUT=1
GRUB_CMDLINE_LINUX_DEFAULT="text"
GRUB_CMDLINE_LINUX="noapic acpi=off"
```
and then run the command:

``` shell
# update-grub
```

## static the network
check the network link name:

``` shell
$ ip a
```

edit the /etc/network/interfaces:
``` shell
auto enp2s0
allow-hotplug enp2s0
iface enp2s0 inet static
broadcast 192.168.1.255
address 192.168.1.100
netmask 255.255.255.0
gateway 192.168.1.1
dns-nameservers 192.168.1.1
```
disable the NetworkManager, and enable networking

``` shell
$ sudo systemctl disable NetworkManager
$ sudo systemctl stop NetworkManager
$ sudo systemctl enable networking
```

## install docker
``` shell
sudo apt-get install -y \
    apt-transport-https \
    ca-certificates \
    curl \
    gnupg2 \
    software-properties-common
sudo curl -fsSL https://get.docker.com | sudo bash -s docker --mirror Aliyun
## or use this command
curl -fsSL https://download.docker.com/linux/debian/gpg | sudo apt-key add -
sudo add-apt-repository \
   "deb [arch=amd64] https://download.docker.com/linux/debian \
   $(lsb_release -cs) \
   stable"
sudo apt-get update
sudo apt-get install docker-ce docker-ce-cli containerd.io
sudo systemctl enable docker
sudo systemctl start docker
# add the current user to the docker group
sudo usermod -aG docker $USER
```
## trash-cli setting

``` shell
sudo apt-get install trash-cli
sudo rm -rf /[partition mount]/.Trash-`id -u user_name`
sudo mkdir /[partition mount]/.Trash-`id -u user_name`
sudo chown user_name:user_group /[partition mount]/.Trash-`id -u user_name`
```

## fcitx installation
```shell
# sudo apt-get install fcitx-ui-classic fcitx-frontend-fbterm fcitx-frontend-gtk3 im-config
# sudo chmod u+s /usr/bin/fbterm
# im-config
```
set the fcitx as the default input method
Edit ~/.fbtermrc
```shell
input-method=fcitx-fbterm
```
## ifup

``` shell
sudo ifup enp2s0
```
## check the physical cable is up and its state

``` shell
for i in $(ls /sys/class/net)
do
echo $i
cat /sys/class/net/$i/carrier
cat /sys/class/net/$i/operstate
done
```
copy from [How to detect whether a physical cable is connected to network card slot on Linux](https://linuxconfig.org/how-to-detect-whether-a-physical-cable-is-connected-to-network-card-slot-on-linux)

## Network Manager status
``` shell
nmcli dev status
```

## network tool

``` shell
sudo apt install net-tools lshw ethtool
sudo lshw -class network -short
sudo ethtool enp2s0
sudo ethtool -i enp2s0
ip a s enp2s0
```

## rename

``` shell
sudo apt-get install rename inotify-tools jigdo-file calibre dkms rsync k3b
```

## ifdown and ifup

``` shell
sudo systemctl stop ModemManager
sudo systemctl disable ModemManager
sudo ifdown enp2s0
sudo ifup enp2s0
sudo systemctl restart networking
```
## select and install

``` shell
dpkg --get-selections > selectfile

dpkg --set-selections < selectfile
apt-get dselect-upgrade
```

## gnome-tweak-tool swap caps and ctrl key

``` shell
sudo apt-get install gnome-tweak-tool
## then in the gnome-tweak-tool - keyboard --additional setting
# ctrl section, swap ctrl and caps
```
copy from [How do I remap the Caps Lock and Ctrl keys?](https://askubuntu.com/questions/33774/how-do-i-remap-the-caps-lock-and-ctrl-keys)
The gnome 3.30 is much more different than before.

## jmtpfs mtp-tools

``` shell
sudo apt-get install jmtpfs mtp-tools
```
The mount path is `/run/user/$USER/gvfs`
copy from [Where are MTP mounted devices located in the filesystem?](https://askubuntu.com/questions/342319/where-are-mtp-mounted-devices-located-in-the-filesystem)

## VirtualBox can't enable the AMD-V extension. Please disable the KVM kernel extension, recompile your kernel and reboot (VERR_SVM_IN_USE).

```
I am back to a broken minikube start command, after uninstalling KVM to get VirtualBox working again.
```
copy from [virtualbox + AMD cpu's: This computer doesn't have VT-X/AMD-v enabled.](https://github.com/kubernetes/minikube/issues/3706)
the proper solution might be:
[Ticket #18770 (new defect)](https://www.virtualbox.org/ticket/18770)
[Ticket #11577 (new defect)](https://www.virtualbox.org/ticket/11577#comment:15)
[Changeset 79186 in vbox](https://www.virtualbox.org/changeset/79186/vbox)

## pandoc

``` shell
sudo apt-get install -y pandoc
```

## build-dep

``` shell
sudo apt-get build-dep -y emacs erlang git julia tmux nodejs redis-server
```

## install the docker-compose

``` shell
sudo curl -L "https://github.com/docker/compose/releases/download/1.24.1/docker-compose-$(uname -s)-$(uname -m)" -o /usr/local/bin/docker-compose
sudo chmod +x /usr/local/bin/docker-compose
```
copy from [Install Docker Compose](https://docs.docker.com/compose/install/)

## pwgen, ffmpeg, expect

``` shell
sudo apt-get install -y pwgen ffmpeg expect
```

## wireless driver

``` shell
sudo apt-get install -y firmware-atheros
```

## install the nodejs

``` shell
wget -c https://nodejs.org/dist/v13.11.0/node-v13.11.0-linux-x64.tar.xz
sudo tar xaf node-v13.11.0-linux-x64.tar.xz -C /usr/local
echo "PATH=/usr/local/node-v13.11.0-linux-x64/bin:$PATH" >> ~/.zshrc
source ~/.zshrc
```

## install the dart

``` shell
sudo sh -c 'curl https://dl-ssl.google.com/linux/linux_signing_key.pub | apt-key add -'
sudo sh -c 'curl https://storage.googleapis.com/download.dartlang.org/linux/debian/dart_stable.list > /etc/apt/sources.list.d/dart_stable.list'
sudo apt-get update
sudo apt-get install dart
```

## install the rar

``` shell
$ wget -c http://rarlab.com/rar/rarlinux-x64-5.8.0.tar.gz
$ tar xzf rarlinux-x64-5.8.0.tar.gz
$ cd rar
$ sudo make
```

## set nemo as the Default File Manager

``` shell
xdg-mime default nemo.desktop inode/directory application/x-gnome-saved-search
gsettings set org.gnome.desktop.background show-desktop-icons false
gsettings set org.nemo.desktop show-desktop-icons true
```
copy from [How to Install and Make Nemo the Default File Manager in Ubuntu](https://itsfoss.com/install-nemo-file-manager-ubuntu/)

## dmsetup and cryptsetup

``` shell
sudo apt-get install dmsetup cryptsetup lvm2
```
copy from [linux加密文件系统](https://blog.csdn.net/dj0379/article/details/50543939)
also see [How to Encrypt Filesystem using LUKS in Linux ](https://linoxide.com/linux-how-to/encrypt-linux-filesystem/)

## progress, formerly known as 'cv'

``` shell
sudo apt-get install progress
progress -w
```
## stop bluetooth

``` shell
systemctl stop bluetooth
systemctl disable bluetooth
```

## video download helper

``` shell
wget -c https://github.com/mi-g/vdhcoapp/releases/download/v1.3.0/net.downloadhelper.coapp-1.3.0-1_amd64.deb
sudo dpkg -i net.downloadhelper.coapp-1.3.0-1_amd64.deb
```

## fix erlang start observer warning

``` shell
sudo apt-get install -y libcanberra-gtk-module libgail-common
```

## install nodejs in debian
see [How To Install Node.js on Debian 9](https://www.digitalocean.com/community/tutorials/how-to-install-node-js-on-debian-9)

``` shell
curl -sL https://deb.nodesource.com/setup_10.x -o nodesource_setup.sh
sudo bash nodesource_setup.sh
sudo apt install nodejs
```

see [How To Install Node.js on Debian 10](https://www.digitalocean.com/community/tutorials/how-to-install-node-js-on-debian-10)

``` shell
curl -sL https://deb.nodesource.com/setup_12.x -o nodesource_setup.sh
sudo bash nodesource_setup.sh
sudo apt install nodejs
```
then install the yarn:

``` shell
curl -sL https://dl.yarnpkg.com/debian/pubkey.gpg | sudo apt-key add -
echo "deb https://dl.yarnpkg.com/debian/ stable main" | sudo tee /etc/apt/sources.list.d/yarn.list
sudo apt-get update && sudo apt-get install yarn
```

## pinentry-tty

``` shell
sudo apt-get install pinentry-tty
sudo update-alternatives --config pinentry
```

## doxygen

``` shell
sudo apt-get install doxygen
```

## fun commands

``` shell
$ sudo apt-get install cmatrix sl cowsay rig
$ cmatrix
$ sl
$ cowsay "hello world"
$ rig
```

## kvm

``` shell
sudo apt-get install qemu-kvm virt-manager
```

## linux-perf

``` shell
sudo apt-get install -y linux-perf systemtap
```

## software-properties-common
For add-apt-repository command

``` shell
sudo apt-get install software-properties-common
```

## docbook2x, asciidoc-dblatex

``` shell
sudo apt-get install docbook2x asciidoc-dblatex asciidoc
```

## install faketime

``` shell
git clone https://github.com/wolfcw/libfaketime
cd libfaketime
make
sudo make install
faketime '2018-03-27 21:04:52' date
```
copy from [使用faketime修改单个应用程序的系统时间](http://inorz.net/2018/03/26/modifies-the-system-time-for-a-single-application/)

## install adb
adb is used for Android development.
``` shell
sudo apt-get install adb
```

## disable some permission on huawei device

copy from [关闭多任务振动方法](https://club.huawei.com/thread-18084606-1-1.html)
``` shell
adb shell sh /sdcard/Android/data/com.zzzmode.appopsx/opsx.sh
```

## disable Bluetooth support
To disable Bluetooth support in PulseAudio, make sure that the following lines are commented out in the configuration file in use (~/.config/pulse/default.pa or /etc/pulse/default.pa):
```
~/.config/pulse/default.pa

### Automatically load driver modules for Bluetooth hardware
#.ifexists module-bluetooth-policy.so
#load-module module-bluetooth-policy
#.endif

#.ifexists module-bluetooth-discover.so
#load-module module-bluetooth-discover
#.endif

```
copy from [Disable_Bluetooth_support](https://wiki.archlinux.org/index.php/PulseAudio/Troubleshooting#Disable_Bluetooth_support)

## install pass

``` shell
apt-get install -y pass
```
