# debian 11 bullseye installation

## compile linux kernel

``` shell
# apt-get update
# apt-get upgrade -y
# apt-get install -y vim zsh curl nemo git moc moc-ffmpeg-plugin unzip p7zip-full w3m w3m-img proxychains
## in case of `OOM` error
# systemctl set-default multi-user.target
# reboot

# apt-get build-dep -y linux-source
# apt-get install -y firmware-linux firmware-linux-nonfree firmware-linux-free firmware-realtek libncurses6 libncurses-dev linux-source build-essential linux-headers-amd64 libelf-dev libssl-dev dwarves
# cd /usr/src
# xz -d -k linux-patch-5.10-rt.patch.xz
# tar xaf linux-source-5.10.tar.xz
# cd linux-source-5.10
# patch -p1 < ../linux-patch-5.10-rt.patch
# cp /boot/config-5.10.0-amd64 .config
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

## install wx

``` shell
apt-get install libwxgtk3.0-gtk3-dev libwxgtk-webview3.0-gtk3-dev
```


## update from buster to bullseye
change the `/etc/apt/source.list` to be:

```
deb http://mirrors.aliyun.com/debian/ bullseye main non-free contrib
deb-src http://mirrors.aliyun.com/debian/ bullseye main non-free contrib
deb http://mirrors.aliyun.com/debian-security bullseye/updates main
deb-src http://mirrors.aliyun.com/debian-security bullseye/updates main
deb http://mirrors.aliyun.com/debian/ bullseye-updates main non-free contrib
deb-src http://mirrors.aliyun.com/debian/ bullseye-updates main non-free contrib
deb http://mirrors.aliyun.com/debian/ bullseye-backports main non-free contrib
deb-src http://mirrors.aliyun.com/debian/ bullseye-backports main non-free contrib
```
and the run the commands:

``` shell
apt update
apt full-upgrade
```
Some packages need to manual comfirm the install option, for example, the sudoer, sshd_config, these files might be confirmed as we edit the files before.
copy from [Manually upgrade Debian from Buster to Bullseye](https://doc.akito.ooo/link/31#bkmrk-page-title)

## install kube-apiserver

``` shell
sudo apt update
sudo apt install snapd
sudo snap install core

sudo snap install kube-apiserver
```

copy from [Enable snaps on Debian and install kube-apiserver](https://snapcraft.io/install/kube-apiserver/debian)

## enable nonfree repo

``` shell
sudo apt-add-repository non-free
sudo apt-add-repository contrib

wget http://www.deb-multimedia.org/pool/main/d/deb-multimedia-keyring/deb-multimedia-keyring_2016.8.1_all.deb
echo "deb http://www.deb-multimedia.org stable main non-free"| sudo tee /etc/apt/sources.list.d/deb-multimedia.list
```
copy from [Installing Multimedia Codecs on Debian 10](https://linuxhint.com/install_multimedia_codecs_debian_10/)

## pavucontrol

``` shell
sudo apt-get install pavucontrol
pavucontrol
```
`配置` -> `内置音频` -> `模拟立体声双工`
`输出设备` -> `内置音频 模拟立体声` -> Port: Line Out

copy from [ubuntu18.04主机后置耳机没声音](https://forum.ubuntu.org.cn/viewtopic.php?t=487752)

## debian bullseye source mirror

``` shell
deb http://mirrors.163.com/debian/ bullseye main non-free contrib
deb http://mirrors.163.com/debian/ bullseye-updates main non-free contrib
deb http://mirrors.163.com/debian/ bullseye-backports main non-free contrib
deb-src http://mirrors.163.com/debian/ bullseye main non-free contrib
deb-src http://mirrors.163.com/debian/ bullseye-updates main non-free contrib
deb-src http://mirrors.163.com/debian/ bullseye-backports main non-free contrib
deb http://mirrors.163.com/debian-security/ bullseye-security main non-free contrib
deb-src http://mirrors.163.com/debian-security/ bullseye-security main non-free contrib
```

## gpt disk partition

``` shell
$ parted /dev/vdb
(parted) mktable gpt
(parted) print
(parted) mkpart
[ext2]? xfs
起始点？ 1
结束点？ 3TB
(parted) print
```
Not (MBR) msdos.
copy from [Linux下使用gpt给磁盘分区、格式化、挂载](https://blog.51cto.com/wangqh/2089129)

## btrfs

``` shell
sudo apt-get install btrfs-progs
```

## 32G swapfile
LLVM nees 27G memory, but many machines does have the amount of memory.
It can be solved by add swapfile:


``` shell
sudo dd if=/dev/zero of=/swapfile bs=1024 count=32768k
sudo mkswap /swapfile
sudo swapon /swapfile
sudo echo "/swapfile     swap    swap      defaults     0  0" >> /etc/fstab
sudo mount -a
```

copy from [创建交换文件（swapfile）-linux](https://blog.51cto.com/joket/1140156)

Also see [collect2: ld terminated with signal 9 错误解决办法](https://blog.csdn.net/longkg/article/details/12839173)

Also see [collect2: fatal error: ld terminated with signal 9 [Killed]](https://stackoverflow.com/questions/46259776/collect2-fatal-error-ld-terminated-with-signal-9-killed)

```
Increase SWAP disk (8 GB working with me).
Increase Memory (I was on virtual machine, 8 GB).
You need 27 GB (LLVM 6.0.0) free disk space (check with df -h in Terminal).
```

The cargo command:

``` shell
 cargo run --release --verbose --jobs 1
 cargo build --jobs 1
```

## v2ray

``` shell
sudo apt-get install v2ray
sudo systemctl status v2ray.service
sudo systemctl start v2ray.service
sudo systemctl stop v2ray.service
sudo systemctl restart v2ray.service
```
edit `/etc/v2ray/config.json` .
copy from [linux使用v2ray作为客户端](https://lionng.github.io/post/linux-v2ray-client/)

## save pulseaudio setting

``` shell
$ pacmd list-cards | grep "active profile"
active profile: <output:analog-stereo+input:analog-stereo>

$ sudo echo "set-card-profile 1 output:analog-stereo+input:analog-stereo" >> /etc/pulse/default.pa
```
copy from [PulseAudio, Pavucontrol not saving settings after reboot on Ubuntu and Ubuntu based distributions](https://www.mycomputertips.co.uk/213)

Or work as [PulseAudio: Enable 2 output ports by default [solved]](https://bbs.archlinux.org/viewtopic.php?id=263701)
``` shell
$ pacmd list-sinks
1 sink(s) available.
  * index: 2
	name: <alsa_output.pci-0000_00_1f.3.analog-stereo>
	driver: <module-alsa-card.c>
	flags: HARDWARE HW_MUTE_CTRL HW_VOLUME_CTRL DECIBEL_VOLUME LATENCY DYNAMIC_LATENCY
	state: RUNNING
	suspend cause: (none)
	priority: 9039
	volume: front-left: 26424 /  40% / -23.67 dB,   front-right: 26424 /  40% / -23.67 dB
	        balance 0.00
	base volume: 65536 / 100% / 0.00 dB
	volume steps: 65537
	muted: no
	current latency: 131.82 ms
	max request: 24 KiB
	max rewind: 24 KiB
	monitor source: 2
	sample spec: s16le 2ch 44100Hz
	channel map: front-left,front-right
	             立体声
	used by: 1
	linked by: 1
	configured latency: 140.00 ms; range is 0.50 .. 2000.00 ms
	card: 0 <alsa_card.pci-0000_00_1f.3>
	module: 6
	properties:
		alsa.resolution_bits = "16"
		device.api = "alsa"
		device.class = "sound"
		alsa.class = "generic"
		alsa.subclass = "generic-mix"
		alsa.name = "ALCS1200A Analog"
		alsa.id = "ALCS1200A Analog"
		alsa.subdevice = "0"
		alsa.subdevice_name = "subdevice #0"
		alsa.device = "0"
		alsa.card = "0"
		alsa.card_name = "HDA Intel PCH"
		alsa.long_card_name = "HDA Intel PCH at 0xb1220000 irq 140"
		alsa.driver_name = "snd_hda_intel"
		device.bus_path = "pci-0000:00:1f.3"
		sysfs.path = "/devices/pci0000:00/0000:00:1f.3/sound/card0"
		device.bus = "pci"
		device.vendor.id = "8086"
		device.vendor.name = "Intel Corporation"
		device.product.id = "a3f0"
		device.form_factor = "internal"
		device.string = "front:0"
		device.buffering.buffer_size = "352800"
		device.buffering.fragment_size = "176400"
		device.access_mode = "mmap+timer"
		device.profile.name = "analog-stereo"
		device.profile.description = "模拟立体声"
		device.description = "内置音频 模拟立体声"
		module-udev-detect.discovered = "1"
		device.icon_name = "audio-card-pci"
	ports:
		analog-output-lineout: Line Out (priority 9000, latency offset 0 usec, available: no)
			properties:

		analog-output-headphones: Headphones (priority 9900, latency offset 0 usec, available: no)
			properties:
				device.icon_name = "audio-headphones"
	active port: <analog-output-lineout>

 ```
and then list-cards

``` shell
$ pacmd list-cards

1 card(s) available.
    index: 0
	name: <alsa_card.pci-0000_00_1f.3>
	driver: <module-alsa-card.c>
	owner module: 6
	properties:
		alsa.card = "0"
		alsa.card_name = "HDA Intel PCH"
		alsa.long_card_name = "HDA Intel PCH at 0xb1220000 irq 140"
		alsa.driver_name = "snd_hda_intel"
		device.bus_path = "pci-0000:00:1f.3"
		sysfs.path = "/devices/pci0000:00/0000:00:1f.3/sound/card0"
		device.bus = "pci"
		device.vendor.id = "8086"
		device.vendor.name = "Intel Corporation"
		device.product.id = "a3f0"
		device.form_factor = "internal"
		device.string = "0"
		device.description = "内置音频"
		module-udev-detect.discovered = "1"
		device.icon_name = "audio-card-pci"
	profiles:
		input:analog-stereo: 模拟立体声 输入 (priority 65, available: no)
		output:analog-stereo: 模拟立体声 输出 (priority 6500, available: no)
		output:analog-stereo+input:analog-stereo: 模拟立体声双工 (priority 6565, available: no)
		output:analog-surround-21: 模拟环绕 2.1 输出 (priority 1300, available: no)
		output:analog-surround-21+input:analog-stereo: 模拟环绕 2.1 输出 + 模拟立体声 输入 (priority 1365, available: no)
		output:analog-surround-40: 模拟环绕 4.0 输出 (priority 1200, available: no)
		output:analog-surround-40+input:analog-stereo: 模拟环绕 4.0 输出 + 模拟立体声 输入 (priority 1265, available: no)
		output:analog-surround-41: 模拟环绕 4.1 输出 (priority 1300, available: no)
		output:analog-surround-41+input:analog-stereo: 模拟环绕 4.1 输出 + 模拟立体声 输入 (priority 1365, available: no)
		output:analog-surround-50: 模拟环绕 5.0 输出 (priority 1200, available: no)
		output:analog-surround-50+input:analog-stereo: 模拟环绕 5.0 输出 + 模拟立体声 输入 (priority 1265, available: no)
		output:analog-surround-51: 模拟环绕 5.1 输出 (priority 1300, available: no)
		output:analog-surround-51+input:analog-stereo: 模拟环绕 5.1 输出 + 模拟立体声 输入 (priority 1365, available: no)
		output:iec958-stereo: 数字立体声(IEC958) 输出 (priority 5500, available: unknown)
		output:iec958-stereo+input:analog-stereo: 数字立体声(IEC958) 输出 + 模拟立体声 输入 (priority 5565, available: no)
		output:iec958-ac3-surround-51: 数字环绕 5.1(IEC958/AC3) 输出 (priority 300, available: unknown)
		output:iec958-ac3-surround-51+input:analog-stereo: 数字环绕 5.1(IEC958/AC3) 输出 + 模拟立体声 输入 (priority 365, available: no)
		output:hdmi-stereo: Digital Stereo (HDMI) 输出 (priority 5900, available: no)
		output:hdmi-stereo+input:analog-stereo: Digital Stereo (HDMI) 输出 + 模拟立体声 输入 (priority 5965, available: no)
		output:hdmi-surround: Digital Surround 5.1 (HDMI) 输出 (priority 800, available: no)
		output:hdmi-surround+input:analog-stereo: Digital Surround 5.1 (HDMI) 输出 + 模拟立体声 输入 (priority 865, available: no)
		output:hdmi-surround71: Digital Surround 7.1 (HDMI) 输出 (priority 800, available: no)
		output:hdmi-surround71+input:analog-stereo: Digital Surround 7.1 (HDMI) 输出 + 模拟立体声 输入 (priority 865, available: no)
		output:hdmi-stereo-extra1: Digital Stereo (HDMI 2) 输出 (priority 5700, available: no)
		output:hdmi-stereo-extra1+input:analog-stereo: Digital Stereo (HDMI 2) 输出 + 模拟立体声 输入 (priority 5765, available: no)
		output:hdmi-surround-extra1: Digital Surround 5.1 (HDMI 2) 输出 (priority 600, available: no)
		output:hdmi-surround-extra1+input:analog-stereo: Digital Surround 5.1 (HDMI 2) 输出 + 模拟立体声 输入 (priority 665, available: no)
		output:hdmi-surround71-extra1: Digital Surround 7.1 (HDMI 2) 输出 (priority 600, available: no)
		output:hdmi-surround71-extra1+input:analog-stereo: Digital Surround 7.1 (HDMI 2) 输出 + 模拟立体声 输入 (priority 665, available: no)
		output:hdmi-stereo-extra2: Digital Stereo (HDMI 3) 输出 (priority 5700, available: no)
		output:hdmi-stereo-extra2+input:analog-stereo: Digital Stereo (HDMI 3) 输出 + 模拟立体声 输入 (priority 5765, available: no)
		output:hdmi-surround-extra2: Digital Surround 5.1 (HDMI 3) 输出 (priority 600, available: no)
		output:hdmi-surround-extra2+input:analog-stereo: Digital Surround 5.1 (HDMI 3) 输出 + 模拟立体声 输入 (priority 665, available: no)
		output:hdmi-surround71-extra2: Digital Surround 7.1 (HDMI 3) 输出 (priority 600, available: no)
		output:hdmi-surround71-extra2+input:analog-stereo: Digital Surround 7.1 (HDMI 3) 输出 + 模拟立体声 输入 (priority 665, available: no)
		output:hdmi-stereo-extra3: Digital Stereo (HDMI 4) 输出 (priority 5700, available: no)
		output:hdmi-stereo-extra3+input:analog-stereo: Digital Stereo (HDMI 4) 输出 + 模拟立体声 输入 (priority 5765, available: no)
		output:hdmi-surround-extra3: Digital Surround 5.1 (HDMI 4) 输出 (priority 600, available: no)
		output:hdmi-surround-extra3+input:analog-stereo: Digital Surround 5.1 (HDMI 4) 输出 + 模拟立体声 输入 (priority 665, available: no)
		output:hdmi-surround71-extra3: Digital Surround 7.1 (HDMI 4) 输出 (priority 600, available: no)
		output:hdmi-surround71-extra3+input:analog-stereo: Digital Surround 7.1 (HDMI 4) 输出 + 模拟立体声 输入 (priority 665, available: no)
		output:hdmi-stereo-extra4: Digital Stereo (HDMI 5) 输出 (priority 5700, available: no)
		output:hdmi-stereo-extra4+input:analog-stereo: Digital Stereo (HDMI 5) 输出 + 模拟立体声 输入 (priority 5765, available: no)
		output:hdmi-surround-extra4: Digital Surround 5.1 (HDMI 5) 输出 (priority 600, available: no)
		output:hdmi-surround-extra4+input:analog-stereo: Digital Surround 5.1 (HDMI 5) 输出 + 模拟立体声 输入 (priority 665, available: no)
		output:hdmi-surround71-extra4: Digital Surround 7.1 (HDMI 5) 输出 (priority 600, available: no)
		output:hdmi-surround71-extra4+input:analog-stereo: Digital Surround 7.1 (HDMI 5) 输出 + 模拟立体声 输入 (priority 665, available: no)
		off: 关 (priority 0, available: unknown)
	active profile: <output:analog-stereo+input:analog-stereo>
	sinks:
		alsa_output.pci-0000_00_1f.3.analog-stereo/#2: 内置音频 模拟立体声
	sources:
		alsa_output.pci-0000_00_1f.3.analog-stereo.monitor/#2: Monitor of 内置音频 模拟立体声
		alsa_input.pci-0000_00_1f.3.analog-stereo/#3: 内置音频 模拟立体声
	ports:
		analog-input-front-mic: Front Microphone (priority 8500, latency offset 0 usec, available: no)
			properties:
				device.icon_name = "audio-input-microphone"
		analog-input-rear-mic: Rear Microphone (priority 8200, latency offset 0 usec, available: no)
			properties:
				device.icon_name = "audio-input-microphone"
		analog-input-linein: Line In (priority 8100, latency offset 0 usec, available: no)
			properties:

		analog-output-lineout: Line Out (priority 9000, latency offset 0 usec, available: no)
			properties:

		analog-output-headphones: Headphones (priority 9900, latency offset 0 usec, available: no)
			properties:
				device.icon_name = "audio-headphones"
		iec958-stereo-output: Digital Output (S/PDIF) (priority 0, latency offset 0 usec, available: unknown)
			properties:

		hdmi-output-0: HDMI / DisplayPort (priority 5900, latency offset 0 usec, available: no)
			properties:
				device.icon_name = "video-display"
		hdmi-output-1: HDMI / DisplayPort 2 (priority 5800, latency offset 0 usec, available: no)
			properties:
				device.icon_name = "video-display"
		hdmi-output-2: HDMI / DisplayPort 3 (priority 5700, latency offset 0 usec, available: no)
			properties:
				device.icon_name = "video-display"
		hdmi-output-3: HDMI / DisplayPort 4 (priority 5600, latency offset 0 usec, available: no)
			properties:
				device.icon_name = "video-display"
		hdmi-output-4: HDMI / DisplayPort 5 (priority 5500, latency offset 0 usec, available: no)
			properties:
				device.icon_name = "video-display"
```
Then add the following to `~/.config/pulse/default.pa`

```
set-card-profile 0 output:analog-stereo+input:analog-stereo
set-sink-port 2 analog-output-lineout
```

## get disk partition uuid

``` shell
sudo blkid
```

## How to add a Wireless LAN adaptor static IP to Ubuntu that auto connects at startup

``` shell
wpa_passphrase your-ESSID your-wifi-passphrase | sudo tee -a /etc/wpa_supplicant/wpa_supplicant.conf

```

add the following code to the wpa_supplicant file:

``` shell
network={
        ssid="LinuxBabe.Com Network"
        #psk="12345qwert"
        psk=68add4c5fee7dc3d0dac810f89b805d6d147c01e281f07f475a3e0195
        scan_ssid=1
}
```
vim wpa_supplicant.service:

``` shell
vim /etc/systemd/system/wpa_supplicant.service
```
Find the following line.

``` shell
ExecStart=/sbin/wpa_supplicant -u -s -O /run/wpa_supplicant
```
change to be:

``` shell
ExecStart=/sbin/wpa_supplicant -u -s -c /etc/wpa_supplicant/wpa_supplicant.conf -i wlp4s0
Restart=always
```
then start the service

``` shell
sudo systemctl enable wpa_supplicant.service
sudo systemctl start wpa_supplicant.service
```
static the wireless card ip address:

``` shell
sudo vim /etc/systemd/network/static-wifi.network
```
to be :

```
[Match]
Name=wlp4s0

[Route]
Gateway=192.168.1.253
Metric=1024

[Network]
Address=192.168.1.8/24
Gateway=192.168.1.1
DNS=202.96.128.86
DNS=202.96.134.33
DNS=192.168.1.253
```
and create a link file:

``` shell
sudo vim /etc/systemd/network/10-wifi.link
```
add the text to the file:

``` shell
[Match]
MACAddress=a8:4b:05:2b:e8:54

[Link]
NamePolicy=
Name=wlp4s0
```

Then restart the systemd-networkd:

``` shell
sudo systemctl restart systemd-networkd
```
also see [Connect to Wi-Fi From Terminal on Debian 11/10 with WPA Supplicant](https://www.linuxbabe.com/debian/connect-to-wi-fi-from-terminal-on-debian-wpa-supplicant)

Note that, the wireless card static IP address should be on the front of the wire card IP address.

## enable systemd-networkd service
create static network file
``` shell
sudo vim /etc/systemd/network/static-enp1s0.network
```
add the following text to the file:

```
[Match]
Name=enp1s0
[Network]
Address=192.168.5.7/24
Gateway=192.168.5.1
DNS=192.168.253.254
DNS=192.168.5.1
```
then enable the systemd-networkd service

``` shell
sudo systemctl stop networking
sudo systemctl disable networking
sudo systemctl start systemd-networkd
sudo systemctl enable systemd-networkd
```

## ntpdate

``` shell
sudo apt-get install ntpdate
sudo ntpdate pool.ntp.org
sudo hwclock -w
```
In case of v2ray proxy error.

## systemd-resolved.service

``` shell
sudo systemctl enable systemd-resolved.service
sudo systemctl start systemd-resolved.service
sudo mv /etc/resolv.conf /etc/resolv.conf.bak
sudo ln -sf /run/systemd/resolve/stub-resolv.conf /etc/resolv.conf
```
copy from [systemd-resolved](https://wiki.archlinux.org/title/Systemd-resolved)

## nfs

``` shell
sudo apt-get install -y nfs-kernel-server
```

see [MacOS自动挂载nfs服务器共享目录](https://zhuanlan.zhihu.com/p/288594630)
see [如何在Debian 10 Buster上设置NFS服务器](https://blog.csdn.net/allway2/article/details/107546648)

add to /etc/exports:

```
/nfsdata 10.0.0.0/24(rw,root_squash,no_all_squash,sync,insecure)
```
copy from [mac 挂载nfs_MacOS无法挂载NFS Operation not permitted错误解决办法](https://blog.csdn.net/weixin_31572321/article/details/111961316)

or just expose to one special host:

``` shell
/srv/nfsv4/vsc-docker-projects 10.116.0.109/24(rw,sync,root_squash,no_subtree_check,anonuid=1000,anongid=1000,insecure)
```
The share is only exposed to one another host. Hence, insecure should be fine.
copy from [Can't mount NFS share on Mac OS Big Sur shared from Ubuntu 21.04 - rpc.statd not running](https://askubuntu.com/questions/1344687/cant-mount-nfs-share-on-mac-os-big-sur-shared-from-ubuntu-21-04-rpc-statd-not)
