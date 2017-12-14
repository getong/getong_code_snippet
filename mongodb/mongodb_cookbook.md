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
