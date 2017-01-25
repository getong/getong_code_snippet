#debian iptables

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
