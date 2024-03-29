#!/bin/sh
wget -c https://cdimage.debian.org/debian-cd/11.0.0/amd64/iso-dvd/debian-11.0.0-amd64-DVD-1.iso
wget -c https://cdimage.debian.org/debian-cd/11.0.0/amd64/jigdo-dvd/debian-11.0.0-amd64-DVD-2.jigdo
wget -c https://cdimage.debian.org/debian-cd/11.0.0/amd64/jigdo-dvd/debian-11.0.0-amd64-DVD-3.jigdo
wget -c https://cdimage.debian.org/debian-cd/11.0.0/amd64/jigdo-dvd/debian-11.0.0-amd64-DVD-4.jigdo
wget -c https://cdimage.debian.org/debian-cd/11.0.0/amd64/jigdo-dvd/debian-11.0.0-amd64-DVD-5.jigdo
wget -c https://cdimage.debian.org/debian-cd/11.0.0/amd64/jigdo-dvd/debian-11.0.0-amd64-DVD-6.jigdo
wget -c https://cdimage.debian.org/debian-cd/11.0.0/amd64/jigdo-dvd/debian-11.0.0-amd64-DVD-7.jigdo
wget -c https://cdimage.debian.org/debian-cd/11.0.0/amd64/jigdo-dvd/debian-11.0.0-amd64-DVD-8.jigdo
wget -c https://cdimage.debian.org/debian-cd/11.0.0/amd64/jigdo-dvd/debian-11.0.0-amd64-DVD-9.jigdo
wget -c https://cdimage.debian.org/debian-cd/11.0.0/amd64/jigdo-dvd/debian-11.0.0-amd64-DVD-10.jigdo
wget -c https://cdimage.debian.org/debian-cd/11.0.0/amd64/jigdo-dvd/debian-11.0.0-amd64-DVD-11.jigdo
wget -c https://cdimage.debian.org/debian-cd/11.0.0/amd64/jigdo-dvd/debian-11.0.0-amd64-DVD-12.jigdo
wget -c https://cdimage.debian.org/debian-cd/11.0.0/amd64/jigdo-dvd/debian-11.0.0-amd64-DVD-13.jigdo
wget -c https://cdimage.debian.org/debian-cd/11.0.0/amd64/jigdo-dvd/debian-11.0.0-amd64-DVD-14.jigdo
wget -c https://cdimage.debian.org/debian-cd/11.0.0/amd64/jigdo-dvd/debian-11.0.0-amd64-DVD-15.jigdo
wget -c https://cdimage.debian.org/debian-cd/11.0.0/amd64/jigdo-dvd/debian-11.0.0-amd64-DVD-16.jigdo
wget -c https://cdimage.debian.org/debian-cd/11.0.0/amd64/jigdo-dvd/debian-11.0.0-amd64-DVD-17.jigdo
wget -c https://cdimage.debian.org/debian-cd/11.0.0/amd64/jigdo-dvd/debian-11.0.0-amd64-DVD-18.jigdo
wget -c https://cdimage.debian.org/debian-cd/11.0.0/amd64/jigdo-dvd/debian-11.0.0-amd64-DVD-2.template
wget -c https://cdimage.debian.org/debian-cd/11.0.0/amd64/jigdo-dvd/debian-11.0.0-amd64-DVD-3.template
wget -c https://cdimage.debian.org/debian-cd/11.0.0/amd64/jigdo-dvd/debian-11.0.0-amd64-DVD-4.template
wget -c https://cdimage.debian.org/debian-cd/11.0.0/amd64/jigdo-dvd/debian-11.0.0-amd64-DVD-5.template
wget -c https://cdimage.debian.org/debian-cd/11.0.0/amd64/jigdo-dvd/debian-11.0.0-amd64-DVD-6.template
wget -c https://cdimage.debian.org/debian-cd/11.0.0/amd64/jigdo-dvd/debian-11.0.0-amd64-DVD-7.template
wget -c https://cdimage.debian.org/debian-cd/11.0.0/amd64/jigdo-dvd/debian-11.0.0-amd64-DVD-8.template
wget -c https://cdimage.debian.org/debian-cd/11.0.0/amd64/jigdo-dvd/debian-11.0.0-amd64-DVD-9.template
wget -c https://cdimage.debian.org/debian-cd/11.0.0/amd64/jigdo-dvd/debian-11.0.0-amd64-DVD-10.template
wget -c https://cdimage.debian.org/debian-cd/11.0.0/amd64/jigdo-dvd/debian-11.0.0-amd64-DVD-11.template
wget -c https://cdimage.debian.org/debian-cd/11.0.0/amd64/jigdo-dvd/debian-11.0.0-amd64-DVD-12.template
wget -c https://cdimage.debian.org/debian-cd/11.0.0/amd64/jigdo-dvd/debian-11.0.0-amd64-DVD-13.template
wget -c https://cdimage.debian.org/debian-cd/11.0.0/amd64/jigdo-dvd/debian-11.0.0-amd64-DVD-14.template
wget -c https://cdimage.debian.org/debian-cd/11.0.0/amd64/jigdo-dvd/debian-11.0.0-amd64-DVD-15.template
wget -c https://cdimage.debian.org/debian-cd/11.0.0/amd64/jigdo-dvd/debian-11.0.0-amd64-DVD-16.template
wget -c https://cdimage.debian.org/debian-cd/11.0.0/amd64/jigdo-dvd/debian-11.0.0-amd64-DVD-17.template
wget -c https://cdimage.debian.org/debian-cd/11.0.0/amd64/jigdo-dvd/debian-11.0.0-amd64-DVD-18.template


## 由于下载的iso流量很大，aliyun会对个人ip进行拒绝访问，出现403错误。
## 这个时候就要更换/etc/apt/sources.list里面的源，aliyun、ustc，163的源都是可以使用的。
jigdo-lite --noask https://cdimage.debian.org/debian-cd/11.0.0/amd64/jigdo-dvd/debian-11.0.0-amd64-DVD-2.jigdo
jigdo-lite --noask https://cdimage.debian.org/debian-cd/11.0.0/amd64/jigdo-dvd/debian-11.0.0-amd64-DVD-3.jigdo
jigdo-lite --noask https://cdimage.debian.org/debian-cd/11.0.0/amd64/jigdo-dvd/debian-11.0.0-amd64-DVD-4.jigdo
jigdo-lite --noask https://cdimage.debian.org/debian-cd/11.0.0/amd64/jigdo-dvd/debian-11.0.0-amd64-DVD-5.jigdo
jigdo-lite --noask https://cdimage.debian.org/debian-cd/11.0.0/amd64/jigdo-dvd/debian-11.0.0-amd64-DVD-6.jigdo
jigdo-lite --noask https://cdimage.debian.org/debian-cd/11.0.0/amd64/jigdo-dvd/debian-11.0.0-amd64-DVD-7.jigdo
jigdo-lite --noask https://cdimage.debian.org/debian-cd/11.0.0/amd64/jigdo-dvd/debian-11.0.0-amd64-DVD-8.jigdo
jigdo-lite --noask https://cdimage.debian.org/debian-cd/11.0.0/amd64/jigdo-dvd/debian-11.0.0-amd64-DVD-9.jigdo
jigdo-lite --noask https://cdimage.debian.org/debian-cd/11.0.0/amd64/jigdo-dvd/debian-11.0.0-amd64-DVD-10.jigdo
jigdo-lite --noask https://cdimage.debian.org/debian-cd/11.0.0/amd64/jigdo-dvd/debian-11.0.0-amd64-DVD-11.jigdo
jigdo-lite --noask https://cdimage.debian.org/debian-cd/11.0.0/amd64/jigdo-dvd/debian-11.0.0-amd64-DVD-12.jigdo
jigdo-lite --noask https://cdimage.debian.org/debian-cd/11.0.0/amd64/jigdo-dvd/debian-11.0.0-amd64-DVD-13.jigdo
jigdo-lite --noask https://cdimage.debian.org/debian-cd/11.0.0/amd64/jigdo-dvd/debian-11.0.0-amd64-DVD-14.jigdo
jigdo-lite --noask https://cdimage.debian.org/debian-cd/11.0.0/amd64/jigdo-dvd/debian-11.0.0-amd64-DVD-15.jigdo
jigdo-lite --noask https://cdimage.debian.org/debian-cd/11.0.0/amd64/jigdo-dvd/debian-11.0.0-amd64-DVD-16.jigdo
jigdo-lite --noask https://cdimage.debian.org/debian-cd/11.0.0/amd64/jigdo-dvd/debian-11.0.0-amd64-DVD-17.jigdo
jigdo-lite --noask https://cdimage.debian.org/debian-cd/11.0.0/amd64/jigdo-dvd/debian-11.0.0-amd64-DVD-18.jigdo

wget -c https://cdimage.debian.org/debian-cd/11.0.0/source/iso-dvd/debian-11.0.0-source-DVD-1.iso
wget -c https://cdimage.debian.org/debian-cd/11.0.0/source/iso-dvd/debian-11.0.0-source-DVD-2.iso
wget -c https://cdimage.debian.org/debian-cd/11.0.0/source/iso-dvd/debian-11.0.0-source-DVD-3.iso
wget -c https://cdimage.debian.org/debian-cd/11.0.0/source/iso-dvd/debian-11.0.0-source-DVD-4.iso
wget -c https://cdimage.debian.org/debian-cd/11.0.0/source/iso-dvd/debian-11.0.0-source-DVD-5.iso
wget -c https://cdimage.debian.org/debian-cd/11.0.0/source/iso-dvd/debian-11.0.0-source-DVD-6.iso
wget -c https://cdimage.debian.org/debian-cd/11.0.0/source/iso-dvd/debian-11.0.0-source-DVD-7.iso
wget -c https://cdimage.debian.org/debian-cd/11.0.0/source/iso-dvd/debian-11.0.0-source-DVD-8.iso
wget -c https://cdimage.debian.org/debian-cd/11.0.0/source/iso-dvd/debian-11.0.0-source-DVD-9.iso
wget -c https://cdimage.debian.org/debian-cd/11.0.0/source/iso-dvd/debian-11.0.0-source-DVD-10.iso
wget -c https://cdimage.debian.org/debian-cd/11.0.0/source/iso-dvd/debian-11.0.0-source-DVD-11.iso
wget -c https://cdimage.debian.org/debian-cd/11.0.0/source/iso-dvd/debian-11.0.0-source-DVD-12.iso
wget -c https://cdimage.debian.org/debian-cd/11.0.0/source/iso-dvd/debian-11.0.0-source-DVD-13.iso
wget -c https://cdimage.debian.org/debian-cd/11.0.0/source/iso-dvd/debian-11.0.0-source-DVD-14.iso
wget -c https://cdimage.debian.org/debian-cd/11.0.0/source/iso-dvd/debian-11.0.0-source-DVD-15.iso
wget -c https://cdimage.debian.org/debian-cd/11.0.0/source/iso-dvd/debian-11.0.0-source-DVD-16.iso
wget -c https://cdimage.debian.org/debian-cd/11.0.0/source/iso-dvd/debian-11.0.0-source-DVD-17.iso

wget -c https://cdimage.debian.org/debian-cd/11.0.0/source/iso-dvd/SHA256SUMS -O debian-source-sha256sum.txt

wget -c https://cdimage.debian.org/cdimage/unofficial/non-free/cd-including-firmware/11.0.0+nonfree/amd64/iso-dvd/firmware-11.0.0-amd64-DVD-1.iso
wget -c https://cdimage.debian.org/cdimage/unofficial/non-free/cd-including-firmware/11.0.0+nonfree/amd64/iso-dvd/SHA256SUMS -O debian-11-nonfree-iso-sha256sums.txt
