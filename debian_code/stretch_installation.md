# stretch amd64 installation

## Be carefull when type a command in the console.

## update and upgrade system
```shell
# apt-get update
# apt-get upgrade -y
```

## 安装过程不要使用网络
1. 国内网络不好，下载镜像源会比较慢，会出现下载不了的情况，耗费大量时间不值得。
2. 下载的更新软件里面可能会有 _linux_ , 这个在安装过程中会出现安装不了的情况。

## 编译linux内核

``` shell
# apt-get build-dep -y linux
# apt-get install -y firmware-amd-graphics firmware-linux-free firmware-linux-nonfree libncurses5 libncurses5-dev linux-source build-essential linux-headers-amd64 vim git subversion curl
# cd /usr/src
# xz -d -k linux-patch-4.9-rt.patch.xz
# tar xaf linux-source-4.9.tar.xz
# cd linux-source-4.9
# patch -p1 < ../linux-patch-4.9-rt.patch
# cp /boot/config-4.9.0-3-amd64 .config
```

修改.config文件，CONFIG_SATA_PMP=n

``` shell
make menuconfig
```
直接退出, 添加make.sh

``` shell
#!/bin/sh
make -j`nproc` bzImage && make -j`nproc` modules && make modules_install && make headers_install && make install && shutdown -h now
echo "compile kernel failed " >> failed.txt
shutdown -h now
```

启动运行：

``` shell
chmod +x make.sh
nohup ./make.sh &

```
## update grub

```shell
# cat /etc/default/grub
# If you change this file, run 'update-grub' afterwards to update
# /boot/grub/grub.cfg.
# For full documentation of the options in this file, see:
#   info -f grub -n 'Simple configuration'

GRUB_DEFAULT=0
GRUB_TIMEOUT=1
GRUB_DISTRIBUTOR=`lsb_release -i -s 2> /dev/null || echo Debian`
GRUB_CMDLINE_LINUX_DEFAULT="text"
GRUB_CMDLINE_LINUX="noapic acpi=off"

# Uncomment to enable BadRAM filtering, modify to suit your needs
# This works with Linux (no patch required) and with any kernel that obtains
# the memory map information from GRUB (GNU Mach, kernel of FreeBSD ...)
#GRUB_BADRAM="0x01234567,0xfefefefe,0x89abcdef,0xefefefef"

# Uncomment to disable graphical terminal (grub-pc only)
#GRUB_TERMINAL=console

# The resolution used on graphical terminal
# note that you can use only modes which your graphic card supports via VBE
# you can see them in real GRUB with the command `vbeinfo'
#GRUB_GFXMODE=640x480

# Uncomment if you don't want GRUB to pass "root=UUID=xxx" parameter to Linux
#GRUB_DISABLE_LINUX_UUID=true

# Uncomment to disable generation of recovery mode menu entries
#GRUB_DISABLE_RECOVERY="true"

# Uncomment to get a beep at grub start
#GRUB_INIT_TUNE="480 440 1"

```
The `grub_timeout` is `1`, and the `noapic acpi=off`.
```shell
# update-grub
```

## install zsh
```shell
# apt-get install -y zsh
$ chsh
/bin/zsh
$ sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
```

## build emacs erlang tmux
```
# apt-get build-dep -y emacs erlang tmux
```
compile emacs, erlang, tmux as the docs

## 找回最大化最小化按钮
See [Bring back "minimize" and "maximize" buttons in titlebars by default](https://labs.riseup.net/code/issues/11270)

``` shell
gsettings set org.gnome.desktop.wm.preferences button-layout ':minimize,maximize,close'
```

## fcitx
```shell
# apt-get install fcitx-ui-classic fcitx-frontend-fbterm
# sudo chmod u+s /usr/bin/fbterm
```
Edit ~/.fbtermrc
```shell
input-method=fcitx-fbterm
```

## systemd multi-user target
```shell
systemctl set-default multi-user.target
```
Temporary enable graphics target
```shell
systemctl isolate graphical.target
```

## install fbv
```shell
$ sudo apt-get install libjpeg-dev
$ git clone https://github.com/godspeed1989/fbv
$ cd fbv
$ ./configure
$ make
$ sudo make install
```
## gem source
```shell
$ sudo gem sources --add https://gems.ruby-china.org/ --remove https://rubygems.org/
$ sudo gem update --system
$ sudo gem install githug
```

## swap caps and ctrl
Edit /etc/default/keyboard
```shell
XKBOPTIONS="ctrl:swapcaps"
```
Then run this:
```shell
$ sudo dpkg-reconfigure -phigh console-setup
```

## network static
See the reference: [NetworkConfiguration](https://wiki.debian.org/NetworkConfiguration)
> As if Stretch, the old network names like eth0, eth1 have gone away as the device name can change. The new names are similar to these: enp6s0, enp8s0, enp0s31f6, enp5s0.
> To find the names of your interfaces you will want to look here:
```shell
$ ls /sys/class/net/
```

Set the local network interface static, write this into /etc/network/interface

``` shell
auto enp0s3
iface enp0s3 inet static
address 192.168.1.56
netmask 255.255.255.0
gateway 192.168.1.1
```

And the `/etc/resolv.conf` is missing, create it and write this into it.

``` shell
nameserver 192.168.1.1
```

Restart the network:

``` shell
# /etc/init.d/networking restart
```

## other software:

``` shell
# apt-get install -y proxychains trash-cli firefox-esr-l10n-zh-cn  moc w3m python-pip apt-transport-https dirmngr w3m-img moc-ffmpeg-plugin fbterm
```

## libssl
The libssl in stretch is `1.1.0f-3` and is the default package.
And the openssl has change some API in 1.1.0, conflicts with 1.0.0 .
Install shadowsocks:

``` shell
# pip install --upgrade shadowsocks
# mkdir -p /etc/shadowsocks
# cd /etc/shadowsocks
# cat ss_client.config
{
        "server": "192.168.1.1",
        "server_port": 443,
        "local_port":1080,
        "password": "123456",
        "timeout":600,
        "method": "aes-256-cfb",
	"fast_open": true,
        "auth": true
}

# sslocal -c ss_client.config -d start

```

The shadowsocks is use libssl 1.0.0, and the stretch is install libssl 1.1.0, something need to be changed.

``` shell
vim /usr/local/lib/python2.7/dist-packages/shadowsocks/crypto/openssl.py
```
change the line 52

``` python
libcrypto.EVP_CIPHER_CTX_cleanup.argtypes = (c_void_p,)
```
to be :

``` python
libcrypto.EVP_CIPHER_CTX_reset.argtypes = (c_void_p,)
```

And the line 111

``` python
libcrypto.EVP_CIPHER_CTX_cleanup(self._ctx)
```
to be

``` python
libcrypto.EVP_CIPHER_CTX_reset(self._ctx)
```
See the reference: [解决openssl升级到1.1.0后shadowsocks服务报错问题](https://blog.lyz810.com/article/2016/09/shadowsocks-with-openssl-greater-than-110/)

## shadowsocks start with systemd
see [system service](../shell_code/systemd.md)

## SwitchyOmega
```shell
wget -c https://github.com/FelisCatus/SwitchyOmega/releases/download/v2.4.6/SwitchyOmega.crx
```

## virtualbox-5.1

``` shell
# echo "deb http://download.virtualbox.org/virtualbox/debian stretch contrib" > /etc/apt/sources.list.d/virtualbox.list
# wget -q https://www.virtualbox.org/download/oracle_vbox_2016.asc -O- |  apt-key add -
# apt-get update
# apt-get install virtualbox-5.1 dkms
# wget -c http://download.virtualbox.org/virtualbox/5.1.22/Oracle_VM_VirtualBox_Extension_Pack-5.1.22-115126.vbox-extpack
# VBoxManage extpack install Oracle_VM_VirtualBox_Extension_Pack-5.1.22-115126.vbox-extpack
## or add replace
# VBoxManage extpack install --replace Oracle_VM_VirtualBox_Extension_Pack-5.1.22-115126.vbox-extpack
```

## virtualbox set default VM location in command line
See [How to change VirtualBox default VM location in command line](https://askubuntu.com/questions/800824/how-to-change-virtualbox-default-vm-location-in-command-line)

``` shell
$ vboxmanage setproperty machinefolder /path/to/directory/
```

## vagrant

``` shell
# wget -c https://releases.hashicorp.com/vagrant/1.9.6/vagrant_1.9.6_x86_64.deb
# dpkg -i vagrant_1.9.6_x86_64.deb
```

## proxychains
Edit /etc/proxychains.conf
```shell
quiet_mode
socks5  127.0.0.1 1080
```

## jigdo-lite not support https url

## install docker in stratch
see [Get Docker CE for Debian](https://docs.docker.com/engine/installation/linux/docker-ce/debian/)
```shell

$sudo apt-get install apt-transport-https ca-certificates curl gnupg2 software-properties-common

$ curl -fsSL https://download.docker.com/linux/debian/gpg | sudo apt-key add -

$ sudo add-apt-repository "deb [arch=amd64] https://download.docker.com/linux/debian $(lsb_release -cs) stable"

$ sudo apt-get update

$ sudo apt-get install docker-ce

$ apt-cache madison docker-ce

$ sudo usermod -aG docker $USER

$ sudo systemctl enable docker
```
