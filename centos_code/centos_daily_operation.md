# centos daily operations

## set timezone without reboot

``` shell
$ timedatectl list-timezones
$ sudo timedatectl set-timezone Asia/Shanghai
$ date
# 2023年 02月 24日 星期五 02:32:16 UTC

$ sudo timedatectl set-timezone America/Los_Angeles

$date
2023年 02月 23日 星期四 18:51:29 PST

## php code
// date_default_timezone_set('UTC');
date_default_timezone_set('America/Los_Angeles');
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
for example, the docker-ce:

``` shell
yum list --showduplicates 'docker-ce' | grep '18.03.*el' | tail -1 | awk '{print $2}'
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

## search available package versions in rpm systems

``` shell
# Redhat based system
yum --showduplicates list <package>

# SuSE system
yum --showduplicates list <package>

# debian based system
apt-cache policy <package>
apt-cache madison <package>
```
copy from [How to check available package versions in rpm systems?](https://unix.stackexchange.com/questions/6263/how-to-check-available-package-versions-in-rpm-systems)

## install the trash-cli

``` shell
sudo yum install python-setuptools
sudo easy_install trash-cli
```

## install specific version of package

``` shell
yum --showduplicate list firefox
yum install [package-name]-[version].[architecture]
yum install firefox-31.5.3-3.el7_1.x86_64
```
copy from [CentOS / RHEL : How to install a specific version of rpm package using YUM](https://www.thegeekdiary.com/centos-rhel-how-to-install-a-specific-version-of-rpm-package-using-yum/)

## reinstall yum
when the yum is broken:

```
There was a problem importing one of the Python modules
required to run yum. The error leading to this problem was:

No module named yum

Please install a package which provides this module, or
verify that the module is installed correctly.

It's possible that the above module doesn't match the
current version of Python, which is:
2.7.5 (default, Apr 21 2021, 14:59:31
```

reinstall yum:
``` shell
// remove the old python
rpm -qa| grep python| xargs rpm -ev --allmatches --nodeps

whereis python |xargs rm -frv


// remove the old yum
rpm -qa|grep yum|xargs rpm -ev --allmatches --nodeps

whereis yum |xargs rm -frv

// download the yum file
wget http://mirrors.ustc.edu.cn/centos/7.9.2009/os/x86_64/Packages/libxml2-python-2.9.1-6.el7.5.x86_64.rpm
wget http://mirrors.ustc.edu.cn/centos/7.9.2009/os/x86_64/Packages/python-2.7.5-89.el7.x86_64.rpm
wget http://mirrors.ustc.edu.cn/centos/7.9.2009/os/x86_64/Packages/python2-rpm-macros-3-34.el7.noarch.rpm
wget http://mirrors.ustc.edu.cn/centos/7.9.2009/os/x86_64/Packages/python-backports-1.0-8.el7.x86_64.rpm
wget http://mirrors.ustc.edu.cn/centos/7.9.2009/os/x86_64/Packages/python-backports-ssl_match_hostname-3.5.0.1-1.el7.noarch.rpm
wget http://mirrors.ustc.edu.cn/centos/7.9.2009/os/x86_64/Packages/python-chardet-2.2.1-3.el7.noarch.rpm
wget http://mirrors.ustc.edu.cn/centos/7.9.2009/os/x86_64/Packages/python-devel-2.7.5-89.el7.x86_64.rpm
wget http://mirrors.ustc.edu.cn/centos/7.9.2009/os/x86_64/Packages/python-iniparse-0.4-9.el7.noarch.rpm
wget http://mirrors.ustc.edu.cn/centos/7.9.2009/os/x86_64/Packages/python-ipaddr-2.1.11-2.el7.noarch.rpm
wget http://mirrors.ustc.edu.cn/centos/7.9.2009/os/x86_64/Packages/python-kitchen-1.1.1-5.el7.noarch.rpm
wget http://mirrors.ustc.edu.cn/centos/7.9.2009/os/x86_64/Packages/python-libs-2.7.5-89.el7.x86_64.rpm
wget http://mirrors.ustc.edu.cn/centos/7.9.2009/os/x86_64/Packages/python-pycurl-7.19.0-19.el7.x86_64.rpm
wget http://mirrors.ustc.edu.cn/centos/7.9.2009/os/x86_64/Packages/python-rpm-macros-3-34.el7.noarch.rpm
wget http://mirrors.ustc.edu.cn/centos/7.9.2009/os/x86_64/Packages/python-setuptools-0.9.8-7.el7.noarch.rpm
wget http://mirrors.ustc.edu.cn/centos/7.9.2009/os/x86_64/Packages/python-srpm-macros-3-34.el7.noarch.rpm
wget http://mirrors.ustc.edu.cn/centos/7.9.2009/os/x86_64/Packages/python-urlgrabber-3.10-10.el7.noarch.rpm
wget http://mirrors.ustc.edu.cn/centos/7.9.2009/os/x86_64/Packages/rpm-python-4.11.3-45.el7.x86_64.rpm
wget http://mirrors.ustc.edu.cn/centos/7.9.2009/os/x86_64/Packages/yum-3.4.3-168.el7.centos.noarch.rpm
wget http://mirrors.ustc.edu.cn/centos/7.9.2009/os/x86_64/Packages/yum-metadata-parser-1.1.4-10.el7.x86_64.rpm
wget http://mirrors.ustc.edu.cn/centos/7.9.2009/os/x86_64/Packages/yum-plugin-aliases-1.1.31-54.el7_8.noarch.rpm
wget http://mirrors.ustc.edu.cn/centos/7.9.2009/os/x86_64/Packages/yum-plugin-fastestmirror-1.1.31-54.el7_8.noarch.rpm
wget http://mirrors.ustc.edu.cn/centos/7.9.2009/os/x86_64/Packages/yum-plugin-protectbase-1.1.31-54.el7_8.noarch.rpm
wget http://mirrors.ustc.edu.cn/centos/7.9.2009/os/x86_64/Packages/yum-utils-1.1.31-54.el7_8.noarch.rpm

// install the yum file
rpm -Uvh --force --nodeps --replacepkgs *.rpm

// update the yum repo
wget -O /etc/yum.repos.d/CentOS-Base.repo https://mirrors.aliyun.com/repo/Centos-7.repo

curl -o /etc/yum.repos.d/epel.repo http://mirrors.aliyun.com/repo/epel-7.repo

```
copy from [centos 7.9 yum重装](https://blog.jairmir.com/index.php/2021/04/21/centos7-9-yum%E9%87%8D%E8%A3%85/)

## yum Peer's Certificate issuer is not recognized.

``` shell
sudo vim /etc/yum.conf
------------------------
sslverify=false
```
copy from [curl: (60) Peer's Certificate issuer is not recognized.](https://stackoverflow.com/questions/47676980/curl-60-peers-certificate-issuer-is-not-recognized)
