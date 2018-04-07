# iptables

## debian iptables

```
iptables is not a system service, but a single command. RedHat-based distros ship with an init script for convenience which allows you to save your iptables configuration, but this is not present on Ubuntu or Debian. You can get similar functionality by using iptables-persistent You can install it with:

sudo apt-get update
sudo apt-get install iptables-persistent
Then save your rules by running:

sudo service iptables-persistent save
And it flush your IPtables rules, effectively disabling your firewall, run:

sudo service iptables-persistent stop
While this article is targeted at Ubuntu, it applies to Debian as well. Check it out for more info on IPtables:

```

## better use deny(drop), not reject

``` shell
iptables -A INPUT -i eth0 -pudp -m multiport --destination-port 135,136 -j DROP
iptables -A OUTPUT -i eth0 -p tcp -m multiport --destination-port 2049,1080,3128 --syn -j REJECT
```

## table
filter, nat, mangle table, the default table is filter

``` shell
iptables -t nat ...
iptables -A INPUT ...
```
## nftables
nfttables is used to replace iptables, and it comes with linux kernel 3.13 .

## ufw
ufw is nice for very basic operations, and it uses iptables behind-the-scenes to do them.
