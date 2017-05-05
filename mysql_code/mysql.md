# mysql
## dump table struct

```
mysqldump --comments -uuser -ppassword -h host_ip --opt database -d --single-transaction | sed 's/ AUTO_INCREMENT=[0-9]*\b//' | sed 's/CREATE TABLE/CREATE TABLE IF NOT EXISTS/g' | sed '/DROP/d' > database_struct.sql
```

## mysql查询结果输出到文件

```
mysql -h host_ip -u user -p database -P 3306 -e "select * from table"  > filename
```

## 查看表的索引

```
mysql> show keys from table_name;

mysql> show index from table_name;

```

## 查看表结构

```
mysql> desc table_name;
```


## 允许user用户远程登陆
用root用户登陆mysql， 在mysql 的shell里面运行下面命令：
```
mysql>grant all on *.* to user@'%' identified by 'password';
```
解析

```
"*.*"：第一个*代表数据库名；第二个*代表表名。这里的意思是所有数据库里的所有表都授权给用户。
user：授予user账号。
"%"：表示授权的用户IP可以指定，这里代表任意的IP地址都能访问MySQL数据库。
"password"：分配账号对应的密码，这里密码替换成user帐号对应的mysql密码。
```
最后，运行下面命令：

```
mysql> flush privileges;
#/etc/init.d/mysqld restart
```
要注意my.cnf里面要配置正确：

```
Just comment out bind-address= altogether. When you do that, it will bind to all addresses.
Also make sure that you don't have skip-networking enabled in your my.cnf.
```

## InnoDB: mmap(137363456 bytes) failed; errno 12
MySQL 5.6 或者 MariaDB 10会占用很多内存。这个错误是因为启动的时候申请大块内存失败导致的。
在my.cnf设置
innodb_buffer_pool_size=8M
