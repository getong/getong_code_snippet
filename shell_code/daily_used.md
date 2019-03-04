# 命令行使用案例
## 获取公网ip
```
curl ifconfig.me
```

## 监控磁盘空间, 间隔1s输出内容
```
watch -n 1 df
```

## 永久删除文件

```
shred -zuv filename
```

## clear terminal
```
reset
```

## dos2unix
```
dos2unix filename
```

## use adduser to add normal user, use useradd to add system user
```
# will create /home/user
adduser user
#not create /home/user2
useradd user2
```

## 列出归属于user的用户的进程和线程

```
$ ps -LF -u user
```

## 找到网关 gateway

```
#netstat -rn
(以0.0.0.0开始的行的gateway是默认网关)
```

## 快速格式化u盘

```
#mkfs.ntfs -f /dev/sdb1

## 参看 /etc/mke2fs.conf
#mkfs.ext4  -T largefile /dev/sdb1
```

## pv拷贝带进度条
```
pv file1 > file2
```

## get shell pid

```
echo $$
```

## The nproc command shows the number of processing units available:

```
$nproc
```

## lowercase filename
To translate uppercase names to lower, you'd use
``` shell
$ rename -n 'y/A-Z/a-z/' filename
$ rename -v 'y/A-Z/a-z/' filename
```
found it on manpage

## get the kill command of zombie process

``` shell
ps -ef | grep defunct | grep -v grep | awk ‘{print “kill -9 ” $2，$3}’

ps -ef | grep defunct | grep -v grep | awk ‘{print “kill -18 ” $3}’
```

## mount iso file

``` shell
mount -t iso9660 -o ro,loop,noauto /your/file.iso /mnt
```

## netstat get network info

``` shell
netstat -putona
```

## say yes to all command

``` shell
yes | apt-get install vim
yes no | command
```


## check the atime, mtime, ctime of a file

``` shell
stat file
```

## number operation
``` shell
#!/bin/bash
echo "Hello World !"
a=3
b=5
val=`expr $a + $b`
echo "Total value : $val"

val=`expr $a - $b`
echo "Total value : $val"

val=`expr $a \* $b`
echo "Total value : $val"

val=`expr $a / $b`
echo "Total value : $val"
```
Note that, multiply operation should be escaped.


## get host ip

``` shell
$ ifconfig
$ ip a
$ hostname -I
```

## awk substr
Print the Nth part(include the Nth) to the end of each line.

``` shell
awk '{print substr($0, index($0, $N))}'
```

## curl post http data

``` shell
curl -d "msg=a&msg1=b" url
curl url -X POST -H 'Content-Type: application/json' -d '{"msg": "a", "msg2": "b"}'
```

## cat words to file
Sometimes, no `vim`, `nano`, you can use `cat` to write words into a file.

``` shell
cat > sources.list << EOF
deb http://mirrors.163.com/debian/ jessie main non-free contrib
deb http://mirrors.163.com/debian/ jessie-updates main non-free contrib
deb http://mirrors.163.com/debian/ jessie-backports main non-free contrib
deb-src http://mirrors.163.com/debian/ jessie main non-free contrib
deb-src http://mirrors.163.com/debian/ jessie-updates main non-free contrib
deb-src http://mirrors.163.com/debian/ jessie-backports main non-free contrib
deb http://mirrors.163.com/debian-security/ jessie/updates main non-free contrib
deb-src http://mirrors.163.com/debian-security/ jessie/updates main non-free contrib
EOF
```


## view linux capacity

``` shell
capsh --print
```
In docker, this will change according to the user.

## dd iso image

``` shell
# /dev/sdb is the ustick mount point
sudo dd if=xxx.iso of=/dev/sdb
```

## sleep n second for copy many files, in case of lager power

``` shell
for i in `ls`
do
cp $i /tmp
sleep 10
done
```

## ln must use absolute path as argument
ln 硬链接文件，inode相同，指向同一个文件。符号链接实际上是一个文本文件，其中包含的有另一个文件的位置信息。
``` shell
# cd /usr/local/otp_src_20.0
# for i in `ls`
do
ln -sf $PWD/$i /usr/bin
done
```

## pip aliyun mirror

``` shell
vi ~/.pip/pip.conf

```
change as below:

``` shell
[global]
trusted-host =  mirrors.aliyun.com
index-url = https://mirrors.aliyun.com/pypi/simple
```

## getfacl

``` shell
$ getfacl install.sh
# file: install.sh
# owner: username
# group: groupname
user::rwx
group::r-x
other::r-x
```

## setfacl

``` shell
$ setfacl -m u:username:rwx filename
```

## < filename command

``` shell
$ < filename cat	# like `cat filename`
$ < filename less	# like `less filename`
```

## iftop
view newwork activity
``` shell
iftop
```

## Linux 查看流量

see [Linux 下大家喜欢用什么命令查看流量？](https://www.zhihu.com/question/19862245)

> nethogs: 按进程查看流量占用
> iptraf: 按连接/端口查看流量
> ifstat: 按设备查看流量
> ethtool: 诊断工具
> tcpdump: 抓包工具
> ss: 连接查看工具
> 其他: dstat, slurm, nload, bmon
> atop nmon collectl vnstat sar iptraf
> 监控总体带宽使用--nload、bmon、slurm、bwm-ng、cbm、speedometer和netload
> 监控总体带宽使用（批量式输出）--vnstat、ifstat、dstat和collectl
> 每个套接字连接的带宽使用--iftop、iptraf、tcptrack、pktstat、netwatch和trafshow
> 每个进程的带宽使用--nethogs

``` shell
dstat -nf
cat /proc/net/dev
```

##  many seconds after now

``` shell
date -d "+1000000000 second now"
```

## shuffle text

``` shell
shuf filename
```

## chattr
Only the `root` can use this command.
Prevent file from deleting.

``` shell
# enable
# chattr +i filename
# disable
# chattr -i filename
```
## lsattr
All users can use it.

``` shell
$ lsattr filename
```

## brctl

``` shell
# brctl show
bridge name     bridge id               STP enabled     interfaces
docker0         8000.024210dc7fcc       no
```

## readlink to find absolute path

``` shell
$ readlink -f filename
```
## join

``` shell
$ join -o1.1 -o2.2, 2.3 file1 file2
```

## check the virtualation of the cpu

``` shell
$ egrep '(vmx|svm)' /proc/cpuinfo
```

## column
format the output text
``` shell
$ mount | column -t
# cat /etc/passwd | column -t -s:
```

## route

``` shell
$ route
Kernel IP routing table
Destination     Gateway         Genmask         Flags Metric Ref    Use Iface
default         gateway         0.0.0.0         UG    0      0        0 enp2s0
172.17.0.0      0.0.0.0         255.255.0.0     U     0      0        0 docker0
192.168.1.0     0.0.0.0         255.255.255.0   U     0      0        0 enp2s0

$ route -n
Kernel IP routing table
Destination     Gateway         Genmask         Flags Metric Ref    Use Iface
0.0.0.0         192.168.1.253   0.0.0.0         UG    0      0        0 enp2s0
172.17.0.0      0.0.0.0         255.255.0.0     U     0      0        0 docker0
192.168.1.0     0.0.0.0         255.255.255.0   U     0      0        0 enp2s0

# add a route table, all packet sent to 192.168.60.0 subnetwork via gateway 192.168.19.1
# route add -net 192.168.60.0 netmask 255.255.255.0 gw 192.168.19.1

# delete a route table
# route del -net 192.168.60.0 netmask 255.255.255.0
```
 In the `Flags`, `U` is usable, `G` is currently used gateway.

## get all commands about network in current linux distribution

``` shell
$ apropos network | less
```

## get what cost so much memory

``` shell
可以使用一下命令查使用内存最多的5个进程
ps -aux | sort -k4nr | head 5
或者
top （然后按下M，注意大写）
可以使用一下命令查使用CPU最多的5个进程
ps -aux | sort -k3nr | head 5
或者
top （然后按下P，注意大写）
top -c 显示进程运行信息列表
top -Hp pid_num 显示一个进程的线程运行信息列表
```
see [linux 查看 占用 内存 最多的 进程](http://xinkang120.blog.163.com/blog/static/19466822320136296271662/)

## linux garbage collection

``` shell
通过修改proc系统的drop_caches清理free的cache
$echo 3 > /proc/sys/vm/drop_caches

drop_caches的详细文档如下：
Writing to this will cause the kernel to drop clean caches, dentries and inodes from memory, causing that memory to become free.
To free pagecache:
* echo 1 > /proc/sys/vm/drop_caches
To free dentries and inodes:
* echo 2 > /proc/sys/vm/drop_caches
To free pagecache, dentries and inodes:
* echo 3 > /proc/sys/vm/drop_caches
As this is a non-destructive operation, and dirty objects are notfreeable, the user should run "sync" first in order to make sure allcached objects are freed.
This tunable was added in 2.6.16.

修改/etc/sysctl.conf 添加如下选项后就不会内存持续增加
vm.dirty_ratio = 1
vm.dirty_background_ratio=1
vm.dirty_writeback_centisecs=2
vm.dirty_expire_centisecs=3
vm.drop_caches=3
vm.swappiness =100
vm.vfs_cache_pressure=163
vm.overcommit_memory=2
vm.lowmem_reserve_ratio=32 32 8
kern.maxvnodes=3

上面的设置比较粗暴，使cache的作用基本无法发挥。需要根据机器的状况进行适当的调节寻找最佳的折衷。
```

## get the process file descriptor

``` shell
ls -al /proc/<PID>/fd
```

## meminfo

``` shell
cat /proc/meminfo
```

## get the absolute path of a file
``` shell
realpath filename
/home/user/filename
```

## find the duplicate files
``` shell
fdupes -r .
```

## get the total tcp connection and memory cost

``` shell
ss -s && free -g
```

## find the top 10  processes which cost memory or cpu most

``` shell
# memory
$ ps aux | sort -rnk 4 | head -n 10

# cpu
$ ps aux | sort -rnk 3 | head -n 10
```

## generate well spelled password

``` shell
apg -a 0 -n 10
```


## flock - manage locks from shell scripts

``` shell
LOCKFILE="/home/xxq/lock_update_check_commit_sh.tmp"

exec 200>$LOCKFILE
flock -n 200 || {
    echo "Another user is doing the same thing，please wait.."
	flock 200
   }
```
see [给shell脚本加锁](http://blog.guoyb.com/2017/09/16/flock/)

## check words checksum

``` shell
echo -n words | md5sum | awk '{print $1}'
echo -n words | sha1sum | awk '{print $1}'
```

## vagrant global-status

view all the vagrant box instance status
``` shell
vagrant global-status
```

## sed

``` shell
i=abc
sed -i "s/\$i/$i/g" filename
```

## Automatically add newline at end of curl response body
copy from [Automatically add newline at end of curl response body](https://stackoverflow.com/questions/12849584/automatically-add-newline-at-end-of-curl-response-body/14614203#14614203)

``` shell
#adding the following to your ~/.curlrc file:

-w "\n"
```
Or:

``` shell
curl example.com; echo
```

## apt-get

``` shell
apt-get policy package_name
```

## vagrant ssh-config

``` shell
vagrant ssh-config
```

## fuser

``` shell
fuser -n tcp|udp -v <port number>[,<remote addresss>[,<remote port>]
$ fuser -n tcp -v 515
                     用户     进程号 权限   命令
6160/tcp:            user_name 12345 F....  beam.smp
```

## netstat
`-a` print all ports, `-p` print process name, `-A` protocol name
``` shell
$ sudo netstat -a -p -A inet
$ sudo netstat -a -p -A inet6
```
## hexdump
print the binary file
```
hexdump -C bin_file
```

## cut video file

``` shell
ffmpeg -ss 0:7:0 -i filename -c copy -t 1:0:0 outfilename
```
From the beginning at `0:7:0` and the total time is `1:0:0`.

## echo and sudo

``` shell
echo "Text I want to write" | sudo tee /path/to/file > /dev/null
sudo sh -c 'echo "Text I want to write" > /path/to/file'
echo "Text I want to write" | sudo tee -a /path/to/file > /dev/null
sudo sh -c 'echo "Text I want to write" >> /path/to/file'
```
This won't work:

``` shell
sudo echo "Text I want to write" > /path/to/file
```
copy from ["sudo echo" does not work together in Ubuntu](https://blogs.oracle.com/joshis/sudo-echo-does-not-work-together-in-ubuntu-another-waste-of-time-issue)

## disable swap

``` shell
#temporary
sysctl -w vm.swappiness=0

#permanently
echo "vm.swappiness = 0">> /etc/sysctl.conf
sysctl -p
```
## 137 signal

When the os process is closed by the 137 signal, the most probably reason is the lack of memory.

## enable swappiness
see [How To Add Swap Space on Ubuntu 16.04](https://www.digitalocean.com/community/tutorials/how-to-add-swap-space-on-ubuntu-16-04)

``` shell
sudo swapon --show
sudo fallocate -l 1G /swapfile
sudo mkswap /swapfile
sudo swapon /swapfile
echo '/swapfile none swap sw 0 0' | sudo tee -a /etc/fstab
sudo sysctl vm.swappiness=10
sudo sysctl vm.vfs_cache_pressure=50

```

## ssh through proxy
It might be coped with ssh_config, but still not know how to use.
Just use proxychains with ssh.
