# 命令行使用案例
## 获取公网ip
```
curl ifconfig.me
```

## 监控磁盘空间
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
