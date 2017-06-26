# stretch amd64 installation

## 安装过程不要使用网络
1. 国内网络不好，下载镜像源会比较慢，会出现下载不了的情况，耗费大量时间不值得。
2. 下载的更新软件里面可能会有 _linux_ , 这个在安装过程中会出现安装不了的情况。

## 编译linux内核

``` shell
apt-get build-dep -y linux
apt-get install -y firmware-amd-graphics firmware-linux-free firmware-linux-nonfree libncurses5 libncurses5-dev \
                   linux-source build-essential linux-headers-amd64 vim \
		   build-essential git subversion curl nemo proxychains trash-cli firefox-esr-l10n-zh-cn \
	moc w3m python-pip
cd /usr/src
xz -d -k linux-patch-4.9-rt.patch.xz
tar xaf linux-source-4.9.tar.xz
cd linux-source-4.9
patch -p1 < ../linux-patch-4.9-rt.patch
cp /boot/config-4.9.0-3-amd64 .config
```

修改.config文件，CONFIG_SATA_PMP=n

``` shell
make menuconfig
```
直接退出, 添加make.sh

``` shell
#!/bin/sh
make -j`nproc` bzImage && make -j`nproc` modules && make modules_install && make headers_install && make install && shutdown -h now echo "compile kernel failed " >> failed.txt shutdown -h now
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
GRUB_CMDLINE_LINUX_DEFAULT="quiet"
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

## libssl
The libssl in stretch is `1.1.0f-3` and is the default package.
And the openssl has change some API in 1.1.0, conflicts with 1.0.0 .
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
