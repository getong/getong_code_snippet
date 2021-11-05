# systemd

## debian config files path

``` shell
/lib/systemd/
```

## centos config files path

``` shell
/usr/lib/systemd/
```


## add systemd service

``` shell
# cat /etc/shadowsocks/config.json
{
    "server":"0.0.0.0",
    "server_port":123456,
    "local_address": "127.0.0.1",
    "local_port":456789,
    "password":"abcabcabc",
    "timeout":300,
    "method":"aes-256-cfb",
    "fast_open": true,
    "workers": 100
}

# cat /lib/systemd/system/shadowsocks.service
[Unit]
Description=Shadowsocks Server
After=network.target

[Service]
PermissionsStartOnly=true
ExecStartPre=/bin/mkdir -p /run/shadowsocks
ExecStartPre=/bin/chown root:root /run/shadowsocks
ExecStart=/usr/local/bin/ssserver -c /etc/shadowsocks/config.json
Restart=on-abort
User=root
Group=root
UMask=0027

[Install]
WantedBy=multi-user.target

# systemctl enable shadowsocks
# systemctl start shadowsocks

```

## sslocal
The sslocal is almost the same with sserver, change the _ExecStart_

``` shell
ExecStart=/usr/local/bin/sslocal -c /etc/shadowsocks/ss_client.config
```

## sysctl config
The `/etc/sysctl.d/local.conf` is

``` shell

# max open files
fs.file-max = 51200
# max read buffer
net.core.rmem_max = 67108864
# max write buffer
net.core.wmem_max = 67108864
# default read buffer
net.core.rmem_default = 65536
# default write buffer
net.core.wmem_default = 65536
# max processor input queue
net.core.netdev_max_backlog = 4096
# max backlog
net.core.somaxconn = 4096

# resist SYN flood attacks
net.ipv4.tcp_syncookies = 1
# reuse timewait sockets when safe
net.ipv4.tcp_tw_reuse = 1
# turn off fast timewait sockets recycling
net.ipv4.tcp_tw_recycle = 0
# short FIN timeout
net.ipv4.tcp_fin_timeout = 30
# short keepalive time
net.ipv4.tcp_keepalive_time = 1200
# outbound port range
net.ipv4.ip_local_port_range = 10000 65000
# max SYN backlog
net.ipv4.tcp_max_syn_backlog = 4096
# max timewait sockets held by system simultaneously
net.ipv4.tcp_max_tw_buckets = 5000
# turn on TCP Fast Open on both client and server side
net.ipv4.tcp_fastopen = 3
# TCP receive buffer
net.ipv4.tcp_rmem = 4096 87380 67108864
# TCP write buffer
net.ipv4.tcp_wmem = 4096 65536 67108864
# turn on path MTU discovery
net.ipv4.tcp_mtu_probing = 1

# for high-latency network
net.ipv4.tcp_congestion_control = hybla

# for low-latency network, use cubic instead
# net.ipv4.tcp_congestion_control = cubic
```

make it worked

``` shell
sysctl --system
```

## serverspeeder

``` shell
https://github.com/91yun/serverspeeder
https://github.com/shadowsocks/shadowsocks-libev
```

## switch to shadowsocks-rust
[shadowsocks-rust](https://github.com/shadowsocks/shadowsocks-rust)

``` shell
cargo install shadowsocks-rust
```
