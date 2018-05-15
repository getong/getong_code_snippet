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

## rand()

``` sql
select ceiling(rand() * 1000);
select floor(rand() * 1000);
update table set column = ceiling(rand() * 1000) where id = 9;

```

## mysql delete user and add user again fail in 5.6
see [ERROR 1396 (HY000): Operation CREATE USER failed for 'username'@'localhost' IDENTIFIED BY 'mypassword';](https://stackoverflow.com/questions/17008610/error-1396-hy000-operation-create-user-failed-for-usernamelocalhost-iden)

``` sql
delete from mysql.user where user='username';
DROP USER 'username'@'%';
delete from mysql.db where user='username';
flush privileges;
CREATE USER 'username'@'localhost' IDENTIFIED BY 'password';

```

## allow user only login from 10.x.x.x subnetwork

``` sql
CREATE USER 'username'@'10.%' IDENTIFIED BY 'password';
create database test_subnetwork;
GRANT ALL PRIVILEGES ON test_subnetwork.* TO 'username'@'10.%' IDENTIFIED BY 'password' WITH GRANT OPTION;
```

## mariadb set root password

``` shell
$ sudo mysql -u root
MariaDB [mysql]> drop user 'root'@'localhost';
MariaDB [mysql]> create user 'root'@'%' identified by 'password';
MariaDB [mysql]> GRANT ALL PRIVILEGES ON *.* TO 'root'@'%';
MariaDB [mysql]> FLUSH PRIVILEGES;
```

## is NULL
select NULL property

``` sql
select * from table where column_name is NULL;

```
__select * from table where column_name = NULL__ not works.


## order by multi column

``` 1c-enterprise
select * from table_name order by id;
select * from table_name order by id, other_column;
select * from table_name order by id desc, other_column;
select * from table_name order by id, other_column desc;
```

## change the password

``` shell
# change /etc/mysql/my.cnf
[mysqld]
skip-grant-tables
```
Then restart mariadb service

``` shell
sudo systemctl restart mariadb.service
# change password in the mysql shell
mysql -u root -p
```
copy from [mariadb或mysql下忘记密码找回](https://blog.csdn.net/wen_dy/article/details/51829296)


## stop mariadb performance schema
mariadb will use 400MB RAM in the performace mode by defult, and with other memory in case of database. Disable it by adding this unde mysqld
```
performance_schema = off
```

In the production environment, use it by recommend. But if it is used under test mode, disable it.

## replace into

``` sql

1. replace into tbl_name(col_name, ...) values(...)
2. replace into tbl_name(col_name, ...) select ...
3. replace into tbl_name set col_name=value, ...
```
use it carefully.
copy from [MySQL replace into 用法（insert into 的增强版）](https://blog.csdn.net/risingsun001/article/details/38977797)
