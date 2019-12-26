# docker default config

## debian default docker config

``` shell
vim /etc/default/docker

```
change the `DOCKER_OPTS` to be "--dns 8.8.8.8 --dns 8.8.4.4"
Then restart docker

``` shell
systemctl restart docker
```

## centos default docker config

edit /etc/sysconfig/docker

``` shell
other_args="--dns 8.8.8.8 --dns 8.8.4.4"
export HTTP_PROXY="http://proxy.company.com:80"
export http_proxy="http://proxy.company.com:80"
export HTTPS_PROXY="http://proxy.company.com:80"
export https_proxy="http://proxy.company.com:80"
```
see the [DNS configuration for CentOS](https://forums.docker.com/t/dns-configuration-for-centos/1863/3)
restart docker

``` shell
service docker restart
```
## Docker image installation directory
```
Ubuntu/Debian: edit your /etc/default/docker file with the -g option: DOCKER_OPTS="-dns 8.8.8.8 -dns 8.8.4.4 -g /mnt"

Fedora/Centos: edit /etc/sysconfig/docker, and add the -g option in the other_args variable: ex. other_args="-g /var/lib/testdir". If there’s more than one option, make sure you enclose them in " ". After a restart, (service docker restart) Docker should use the new directory.
```
copy from [How do I change the Docker image installation directory?](https://forums.docker.com/t/how-do-i-change-the-docker-image-installation-directory/1169)

## docker with proxy

copy from [Configure Docker to use a proxy server](https://docs.docker.com/network/proxy/)
``` shell
#edit ~/.docker/config.json
{
 "proxies":
 {
   "default":
   {
     "http_Proxy": "http://127.0.0.1:3001",
     "noProxy": "*.test.example.com,.example2.com"
   }
 }
}
```

## add docker mirror
add this file, /etc/docker/daemon.json

``` shell
{
        "registry-mirrors": ["http://hub-mirror.c.163.com"]
}
```
then

``` shell
sudo systemctl restart docker
```
copy from [docker设置国内镜像源](https://blog.csdn.net/whatday/article/details/8677060)
