# redis cluster

## ruby redis gem

``` shell
$ sudo gem install redis
```

## redis cluster command:

``` shell
cluster info
cluster nodes
cluster meet
cluster forget
cluster replicate
cluster saveconfig
cluster delslots
cluster addslots
cluster flushslots
cluster setslot <\slot> node <\node_id>
cluster setslot <\slot> migrating <\destination-node-id>
cluster setslot <\slot> importing <\node_id>
cluster setslot <\slot> stable
cluster keyslot <\key>
cluster countkeysinslot <\slot>
cluster getkeysinslot <\slot> <\count>
cluster slaves <\node-id>

```

## configure a cluster using redis commands
see [Redis cluster creation (https://stackoverflow.com/questions/21952813/redis-cluster-creation)

``` shell
cluster addslots 0 1 2 3 ... 16384

cluster meet MASTER_HOST MASTER_PORT
cluster replicate MASTER_ID



$ redis-server 5001/redis.conf

Using Ruby : addslots
$ echo '(0..16383).each{|x| puts "cluster addslots "+x.to_s}' | ruby | redis-cli -c -p 5001 > /dev/null

$ redis-server 5002/redis.conf
$ redis-cli -c -p 5002
127.0.0.1:5002> cluster meet 127.0.0.1 5001
OK
127.0.0.1:5002> cluster replicate 7c38d2e5e76fc4857fe238e34b4096fc9c9f12a5 # node-id of 5001
OK

```
