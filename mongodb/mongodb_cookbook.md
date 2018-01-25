# mongodb cookbook

## delete database

``` shell
use db_name;
db.dropDatabase();
```

## delete collection

``` shell
db.collection.drop();
```

## mongodump

``` shell
mongodump -h Host -p Port -d db_name -o Dir
```

## mongorestore

``` shell
mongorestore -h Host:Port -d db_name Dir
```

## insert integer no default double

``` shell
db.data.update({'name': 'zero'}, {'$set': {'value': NumberInt(0)}})

```
see [MongoDB inserts float when trying to insert integer]https://stackoverflow.com/questions/8218484/mongodb-inserts-float-when-trying-to-insert-integer


## dropDups option is duplicates on unique index in mongodb 2.7.5

## ttl

``` shell
db.collection_name.createIndex({"log_time":1}, {expireAfterSeconds: 10})
```

## garbage collection

Linux cmd
``` shell
$ sudo echo 3 > /proc/sys/vm/drop_caches
```
or restart mongod

``` shell
$ sudo systemctl restart mongod
```
mongodb cmd

``` shell
db.runCommand({closeAllDatabases:1})
```

## explain is Deprecated in the mongo Shell since v3.0

## db.collection.ensureIndex() is now an alias for db.collection.createIndex().

## Yes dropDupes is now deprecated since version 2.7.5 because it was not possible to predict correctly which document would be deleted in the process.

## currentOp

``` shell
db.currentOp();
```

## mongod.config sample

``` shell
# mongod.conf

# for documentation of all options, see:
#   http://docs.mongodb.org/manual/reference/configuration-options/

# where to write logging data.
systemLog:
  destination: file
  logAppend: true
  path: /var/log/mongodb/mongod.log

# Where and how to store data.
storage:
  dbPath: /var/lib/mongo
  journal:
    enabled: true

  engine: "wiredTiger"
#  mmapv1:
  wiredTiger:
    engineConfig:
      cacheSizeGB: 4

# how the process runs
processManagement:
  fork: true  # fork and run in background
  pidFilePath: /var/run/mongodb/mongod.pid  # location of pidfile

# network interfaces
net:
  port: 27017
#  bindIp: 0.0.0.0  # Listen to local interface only, comment to listen on all interfaces.


#security:
#  authorization: 'enabled'
#  keyFile: /opt/mongodb/keyfile

#operationProfiling:

replication:
  replSetName: rs1
#  oplogSize=1024

#sharding:

## Enterprise-Only Options

#auditLog:

#snmp:
```
copy from [mongod.conf yaml file format example](https://www.mysoftkey.com/mongodb/mongod-conf-yaml-file-format-example/)
The `wiredTiger` does not work, not know why.
The reason might be that WiredTiger engine is a new engine, and does not support the old data stored in the MMAPv1 engine.
The old data must be dump and restore back.

## update
```
db.users.update({type: 2}, {$set: {value: 2}});
db.users.update({type: 2}, {$unset: {value: 3}});
db.users.update({type: 2}, {$addToSet: {type: 3}}, false, true);
```

## pretty

```
db.users.find().pretty()
```

## count

```
db.numbers.count()
```
## stats
get the info of a db or a collection
```
db.stats();
db.uses.stats();
```

## createCollection

```
db.createCollection("users");
db.createCollection("users", {size: 20000});
db.createCollection("users", {capped:true, size: 16384, max: 100});
```

## renameCollection

``` javascript
db.old_name.renameCollection("new_name");
```

## createIndex ttl

```
db.revies.createIndex({time_field:1}, {expireAfterSeconds: 3600});
```

## mongodb does not support join

```
db.colletion_name.find({_id: {$in: other_collection_name['collection_id']}});
db.colletion_name.find({_id: other_collection_name['_id']});
```


