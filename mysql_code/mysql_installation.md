# mysql installation

## install in debian stretch

``` shell
$ apt-get install mariadb-server mariadb-client
```

## mysql_secure_installation

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
