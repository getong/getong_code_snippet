# stretch amd64 installation

## 安装过程不要使用网络
1. 国内网络不好，下载镜像源会比较慢，会出现下载不了的情况，耗费大量时间不值得。
2. 下载的更新软件里面可能会有 _linux_ , 这个在安装过程中会出现安装不了的情况。

## 编译linux内核

``` shell
apt-get build-dep -y linux
apt-get install -y firmware-amd-graphics firmware-linux-free firmware-linux-nonfree libncurses5 libncurses5-dev \
                   linux-source build-essential linux-headers-amd64 vim \
				   build-essential git subversion
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
