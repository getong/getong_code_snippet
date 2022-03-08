# install mysql install centos

## get the gpg file
``` shell
wget -c https://repo.mysql.com/RPM-GPG-KEY-mysql-2022
sudo cp RPM-GPG-KEY-mysql-2022 /etc/pki/rpm-gpg/
```
## set the repo

``` shell
sudo vim /etc/yum.repos.d/mysql-community.repo
------------------------------------------
[mysql-connectors-community]
name=MySQL Connectors Community
baseurl=https://mirrors.tuna.tsinghua.edu.cn/mysql/yum/mysql-connectors-community-el7-$basearch/
enabled=1
gpgcheck=1
gpgkey=file:///etc/pki/rpm-gpg/RPM-GPG-KEY-mysql-2022

[mysql-tools-community]
name=MySQL Tools Community
baseurl=https://mirrors.tuna.tsinghua.edu.cn/mysql/yum/mysql-tools-community-el7-$basearch/
enabled=1
gpgcheck=1
gpgkey=file:///etc/pki/rpm-gpg/RPM-GPG-KEY-mysql-2022


[mysql-5.7-community]
name=MySQL 5.7 Community Server
baseurl=https://mirrors.tuna.tsinghua.edu.cn/mysql/yum/mysql-5.7-community-el7-$basearch/
enabled=1
gpgcheck=1
gpgkey=file:///etc/pki/rpm-gpg/RPM-GPG-KEY-mysql-2022
```

## install mysql
enable epel repo as [enable epel](./centos_epel.md)
``` shell
yum install -y mysql-community-libs mysql++
```
