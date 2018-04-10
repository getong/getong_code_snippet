# centos daily operations

## set timezone

``` shell
# timedatectl list-timezones
# timedatectl set-timezone Asia/Shanghai
```

## install fastestmirror

``` shell
yum install yum-plugin-fastestmirror
```
Edit /etc/yum/pluginconf.d/fastestmirror.conf

``` shell
enabled=1
exclude=.gov, facebook
```
The fetch the new packages cache

``` shell
yum clean all
yum makecache

yum check-update
yum upgrade
```

## yumdownloader

``` shell
# yum install yum-utils
# yum check-update

```
Note that the package name and repos output here.

``` shell
# cd /var/cache/yum/x86_64/7/updates/packages ## the updates is the repo that download package
# yumdownloader package_names
```


## install docker using aliyun

see [Docker CE 镜像源站](https://yq.aliyun.com/articles/110806)
``` shell
$ sudo yum-config-manager --add-repo http://mirrors.aliyun.com/docker-ce/linux/centos/docker-ce.repo
$ sudo yum makecache fast
$ sudo yum -y install docker-ce
```


## which  repository provide a particular package?

``` shell
yum list package
```

## sudo in centos
Unlike Debian, the sudo group doest not exist in centos, the sudo group name is wheel
``` shell
# add group user
usermod -aG wheel username

# use visudo to edit sudo user info
visudo

# allow sudo user not type password, uncomment this line in the sudoer file
#%wheel ALL=(ALL)NOPASSWD:ALL
```

## stop firewall in centos 7

``` shell
systemctl stop firewalld
systemctl disable firewalld
```
copy from [How to Stop and Disable Firewalld on CentOS 7](https://www.liquidweb.com/kb/how-to-stop-and-disable-firewalld-on-centos-7/)
