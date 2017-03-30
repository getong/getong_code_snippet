#命令行使用案例
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
##找到网关 gateway

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

## svn add all the deps, including new added files and remove files

```
$ svn add _build
$ svn commit -m 'update new dep' _build
```
