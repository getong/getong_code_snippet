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
