#install the nscd service, to enable the dns services

##in the centos 6.x
```
yum install nscd -y
service nscd start
chkconfig nscd on

```
##in the debian alike system
```
apt-get install -y nscd
```
