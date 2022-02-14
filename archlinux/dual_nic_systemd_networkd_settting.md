# dual nic systemd-networkd setting

## get route info
``` shell
ip route show
```

## two nic address info

device	ip address	        netmask     	gateway
eth0	111.111.111.111 	255.255.255.0	111.111.111.1
eth1	222.222.222.222	    255.255.255.0	222.222.222.1


## set the network card
in the /etc/systemd/network/

eth0.network:

```
[Match]
Name=eth0

[Network]
DHCP=no
Address=111.111.111.111/24
Gateway=111.111.111.1
DNS=114.114.114.114
DNS=8.8.8.8

# 从这往上的配置和刚才的一样，只是去掉了注释。
# 下面多加了几个配置。

# 配置子路由表的默认网关，该小节等同于：
# ip route add default via 111.111.111.1 dev eth0 table 111
# Table 字段表示该子路由表的数字 ID。
[Route]
Table=111
Gateway=111.111.111.1

# 配置子路由表的本网段路由，该小节等同于：
# ip route add 111.111.111.0/24 dev eth0 src 111.111.111.111 table 111
[Route]
Table=111
Destination=111.111.111.0/24
Source=111.111.111.111

# 配置到该子路由表的策略，该小节等同于下面两个步骤：
# echo "100 111" >> /etc/iproute2/rt_tables && \
# ip rule add from 111.111.111.111 table 111
[RoutingPolicyRule]
Table=111
Priority=100
From=111.111.111.111
```


eth1.network

```
[Match]
Name=eth1

[Network]
DHCP=no
Address=222.222.222.222/24
DNS=114.114.114.114
DNS=8.8.8.8

# 依然是在原 eth1 的配置下面多加了几个配置。

# 配置子路由表的默认网关，该小节等同于：
# ip route add default via 222.222.222.1 dev eth0 table 222
# Table 字段表示该子路由表的数字 ID。
[Route]
Table=222
Gateway=222.222.222.1

# 配置子路由表的本网段路由，该小节等同于：
# ip route add 222.222.222.0/24 dev eth0 src 222.222.222.222 table 222
[Route]
Table=222
Destination=222.222.222.0/24
Source=222.222.222.222

# 配置到该子路由表的策略，该小节等同于：
# echo "200 222" >> /etc/iproute2/rt_tables && \
# ip rule add from 222.222.222.222 table 222
[RoutingPolicyRule]
Table=222
Priority=200
From=222.222.222.222
```

copy from [Linux 上使用 systemd-networkd 服务配置策略路由](https://blog.systemctl.top/2017/2017-12-23_policy-routing-with-systemd-networkd/)
