# elasticsearch configure

## File descriptor

``` shell
sudo echo "elasticsearch - nofile 65536" >> /etc/security/limits.conf
```

## virtual memory

``` shell
sudo sysctl -w vm.max_map_count=262144
sudo sysctl -p
cat /proc/sys/vm/max_map_count
```
