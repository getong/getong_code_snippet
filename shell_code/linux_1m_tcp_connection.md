# linux 1m tcp connection

## sysctl setting

``` shell
ulimit -n 20000500
echo 20000500 > /proc/sys/fs/nr_open

sysctl -w net.ipv4.ip_local_port_range="500   65535"
echo 3000000 > /proc/sys/fs/nr_open
ulimit -n 2000000

echo 1 > /proc/sys/net/ipv4/tcp_tw_recycle
sysctl -w net.ipv4.tcp_mem="383865   511820   2303190"
sysctl -w net.ipv4.tcp_rmem="1024   4096   16384"
sysctl -w net.ipv4.tcp_wmem="1024   4096   16384"
sysctl -w net.ipv4.tcp_moderate_rcvbuf="0"
```
copy from [how to reach 1M concurrent TCP connections?](https://serverfault.com/questions/962874/how-to-reach-1m-concurrent-tcp-connections)

also see [Scaling to 12 Million Concurrent Connections: How MigratoryData Did It](https://migratorydata.com/2013/10/10/scaling-to-12-million-concurrent-connections-how-migratorydata-did-it/)

## /etc/sysctl.conf ,  /etc/security/limits.conf

```
## /etc/security/limits.conf
## System Limits for FDs
## "nofile" is "Number of Open Files"
## This is the cap on number of FDs in use concurrently.
## Set nofile to the max value of 1,048,576.

#<user>     <type>    <item>     <value>
*           soft      nofile     1048576
*           hard      nofile     1048576
root        soft      nofile     1048576
root        hard      nofile     1048576
```
and the sysctl.conf

```
## /etc/sysctl.conf
## Increase Outbound Connections
## Good for a service mesh and proxies like
## Nginx/Envoy/HAProxy/Varnish and applications that
## need long-lived connections.
## Careful not to set the range wider as you will impact
## running application ports in heavy usage situations.
net.ipv4.ip_local_port_range = 12000 65535


## Increase Inbound Connections
## Allows for +1M more FDs
## An FD is an integer value used as a traffic I/O pointer
## on a connection with a Client.
## The FD Int value is used to traffic packets between
## User and Kernel Space.
fs.file-max = 1048576
```

dockerfile

``` dockerfile
# Dockerfile
FROM alpine:3.8
RUN echo 'net.ipv4.ip_local_port_range = 12000 65535' >> /etc/sysctl.conf
RUN echo 'fs.file-max = 1048576' >> /etc/sysctl.conf
RUN mkdir /etc/security/
RUN echo '*                soft    nofile          1048576' >> /etc/security/limits.conf
RUN echo '*                hard    nofile          1048576' >> /etc/security/limits.conf
RUN echo 'root             soft    nofile          1048576' >> /etc/security/limits.conf
RUN echo 'root             hard    nofile          1048576' >> /etc/security/limits.conf
CMD echo '+1M Connections' # your application here
```

docker build command

``` shell
docker build -t one-million ./
docker run --ulimit nofile=1048576:1048576 one-million
```

docker compose file

``` shell
# docker-compose.yml
version: "3.7"
services:
  myserver:
    command: echo '+1M TCP Connections' # your application here

    build:
      context: .
      dockerfile: Dockerfile

    ulimits:
      nofile:
        soft: 1048576
        hard: 1048576

    sysctls:
      net.ipv4.ip_local_port_range: 12000 65535
```
copy from [EC2 Tuning for +1M TCP Connections using Linux](https://www.linkedin.com/pulse/ec2-tuning-1m-tcp-connections-using-linux-stephen-blum)
