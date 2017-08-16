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
