# mysql installation

## install in debian stretch

``` shell
$ apt-get install mariadb-server mariadb-client
```

## mysql_secure_installation, for mysql 5.6 only

set the root user's password
``` shell
$ mysql_secure_installation
```

## can't login as mysql user root from normal user account
copy from [can't login as mysql user root from normal user account in ubuntu 16.04](https://askubuntu.com/questions/766334/cant-login-as-mysql-user-root-from-normal-user-account-in-ubuntu-16-04)

login using root user account
``` shell
# mysql -u root -p
mysql> SELECT User,Host FROM mysql.user;
mysql> DROP USER 'root'@'localhost';
mysql> CREATE USER 'root'@'%' IDENTIFIED BY 'mysql_pw';
mysql> GRANT ALL PRIVILEGES ON *.* TO 'root'@'%' WITH GRANT OPTION;
mysql> FLUSH PRIVILEGES;
mysql> exit
```

## Can't connect to MySQL server on 'DOMAIN' (111)
copy from [Can't connect to MySQL server on 'DOMAIN' (111)](https://ubuntuforums.org/showthread.php?t=2242435)
in my.conf, comment the line `bind-address = 127.0.0.1`
``` shell
#bind-address = 127.0.0.1
```

## start and stop mariadb

``` shell
sudo systemctl stop mariadb.service
sudo systemctl start mariadb.service
sudo systemctl enable mariadb.service
```

## mariadb on debian stretch

``` shell
sudo apt-get install software-properties-common dirmngr
sudo apt-key adv --recv-keys --keyserver keyserver.ubuntu.com 0xF1656F24C74CD1D8
sudo add-apt-repository 'deb [arch=amd64,i386,ppc64el] http://mirrors.tuna.tsinghua.edu.cn/mariadb/repo/10.2/debian stretch main'
sudo apt-get update
sudo apt-get install mariadb-server
```
the source mirror:

``` shell
# MariaDB 10.2 repository list - created 2018-04-09 08:23 UTC
# http://downloads.mariadb.org/mariadb/repositories/
deb [arch=amd64,i386,ppc64el] http://mirrors.tuna.tsinghua.edu.cn/mariadb/repo/10.2/debian stretch main
deb-src http://mirrors.tuna.tsinghua.edu.cn/mariadb/repo/10.2/debian stretch main
```
copy from [mariadb installation](https://downloads.mariadb.org/mariadb/repositories/#mirror=tuna&distro=Debian&distro_release=stretch--stretch&version=10.2)

## mysql 8 installation

``` shell
wget https://mirrors.tuna.tsinghua.edu.cn/mysql/yum/mysql80-community-el7/mysql80-community-release-el7-3.noarch.rpm
rpm -ivh mysql80-community-release-el7-3.noarch.rpm
```
Then change the /etc/yum.repos.d/mysql-community.repo file according to [Mysql Community Edition 镜像使用帮助](https://mirrors.tuna.tsinghua.edu.cn/help/mysql/)

```
[mysql-connectors-community]
name=MySQL Connectors Community
baseurl=https://mirrors.tuna.tsinghua.edu.cn/mysql/yum/mysql-connectors-community-el7/
enabled=1
gpgcheck=1
gpgkey=file:///etc/pki/rpm-gpg/RPM-GPG-KEY-mysql

[mysql-tools-community]
name=MySQL Tools Community
baseurl=https://mirrors.tuna.tsinghua.edu.cn/mysql/yum/mysql-tools-community-el7/
enabled=1
gpgcheck=1
gpgkey=file:///etc/pki/rpm-gpg/RPM-GPG-KEY-mysql

[mysql56-community]
name=MySQL 5.6 Community Server
baseurl=https://mirrors.tuna.tsinghua.edu.cn/mysql/yum/mysql56-community-el7/
enabled=0
gpgcheck=1
gpgkey=file:///etc/pki/rpm-gpg/RPM-GPG-KEY-mysql

[mysql57-community]
name=MySQL 5.7 Community Server
baseurl=https://mirrors.tuna.tsinghua.edu.cn/mysql/yum/mysql57-community-el7/
enabled=1
gpgcheck=1
gpgkey=file:///etc/pki/rpm-gpg/RPM-GPG-KEY-mysql

[mysql80-community]
name=MySQL 8.0 Community Server
baseurl=https://mirrors.tuna.tsinghua.edu.cn/mysql/yum/mysql80-community-el7/
enabled=1
gpgcheck=1
gpgkey=file:///etc/pki/rpm-gpg/RPM-GPG-KEY-mysql
```
And run the command:

``` shell
yum makecache fast
yum install -y mysql-community-server

systemctl start mysqld
grep -R "password" /var/log/mysqld.log
A temporary password is generated for root@localhost: xxxxx
```
The xxxxx is your temporary password.
Change the mysql password:

``` shell
mysql -uroot -pxxx
> alter user 'root'@'localhost' identified by '123456'; ## change your password here
> update mysql.user set host='%' where user='root';
> flush privileges;
```

or change mysql password:

``` shell
> ALTER USER 'root'@'localhost' IDENTIFIED BY 'QHRIy?ryk6oH';
> grant all privileges on *.* to 'root'@'localhost'  with grant option;
> flush privileges;
```

## insecure mysql password

``` shell
#!/bin/bash

mysql -h "$MYSQL_HOSTNAME" -u "$MYSQL_USERNAME" "$MYSQL_DATABASE" --password="$MYSQL_PASSWORD" "$@"
```
to:

``` shell
#!/bin/bash

MYSQL_PWD="$MYSQL_PASSWORD" mysql -h "$MYSQL_HOSTNAME" -u "$MYSQL_USERNAME" "$MYSQL_DATABASE" "$@"
```

copy from [MySQL: Using a password on the command line interface can be insecure](https://www.codingwithjesse.com/blog/mysql-using-a-password-on-the-command-line-interface-can-be-insecure/)

## windows 5.7 mysql
```
cd mysqld_bin_directory
# run as administrator
mysqld --remove
mysqld --install
mysqld --initialize
net start mysql
# C:\Program Files\MySQL\MySQL Server <version number>\data\<computer name>.err
# or local directory <computer name>.err
# win10.err
# 2023-03-08T07:03:18.458800Z 1 [Note] A temporary password is generated for root@localhost: *Y3Io7s_fkxv
mysql -u root -p*Y3Io7s_fkxv
> alter user 'root'@'localhost' identified by '123456';
> update mysql.user set host='%' where user='root';
> flush privileges;
```
