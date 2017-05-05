# centos 6.x install python 2.7

```
wget -c https://centos6.iuscommunity.org/ius-release.rpm
rpm -ivh ius-release.rpm

yum -y install python27 python27-devel python27-pip python27-setuptools python27-virtualenv --enablerepo=ius
```
