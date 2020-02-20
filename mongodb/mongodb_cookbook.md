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

## mongodb start with configuration file

``` shell
mongod --config /etc/mongod.conf
mongod -f /etc/mongod.conf
```

## mongodb running in docker
``` shell
chmod -R go+w .

docker run -d --privileged --restart=always --network host -v $PWD/db:/data/db  -v $PWD/config:/mongodb_config --name my_own_mongo mongo:4.2-bionic -f /mongodb_config/config_file
docker run -d --network some-network --name some-mongo -p 27017:27017 -p 8081:8081 -e MONGO_INITDB_ROOT_USERNAME=mongoadmin -e MONGO_INITDB_ROOT_PASSWORD=secret mongo:4.2-bionic
docker run -d --network some-network --name some-mongo -p 27017:27017 -p 8082:8081 -v $PWD:/mongo_config -e MONGO_INITDB_ROOT_USERNAME_FILE=/mongo_config/user_name_file -e MONGO_INITDB_ROOT_PASSWORD_FILE=/mongo_config/secret_file mongo:4.2-bionic
docker run -it --rm mongo:4.2-bionic mongo --host 192.168.1.1 --port 27017 -u mongoadmin -p secret
```

Is there  a better way to do this?


## Deploy a MongoDB Cluster in 9 steps Using Docker

``` shell
# step 1
root@node*:/# export node1=10.11.32.174
root@node*:/# export node2=10.11.33.37
root@node*:/# export node3=10.11.31.176

# step 2
root@node*:/# mkdir -p /home/core
root@node*:/# cd /home/core
root@node*:/# openssl rand -base64 741 > mongodb-keyfile
root@node*:/# chmod 600 mongodb-keyfile
root@node*:/# sudo chown 999 mongodb-keyfile

# step 3
root@node1:/# docker run --name mongo \
-v /home/core/mongo-files/data:/data/db \
-v /home/core/mongo-files:/opt/keyfile \
--hostname="node1.example.com" \
-p 27017:27017 \
-d mongo:2.6.5 --smallfiles

root@node1:/# docker exec -it mongo /bin/bash
root@node1:/# mongo
> use admin
switched to db admin
> db.createUser( {
     user: "siteUserAdmin",
     pwd: "password",
     roles: [ { role: "userAdminAnyDatabase", db: "admin" } ]
   });
> db.createUser( {
     user: "siteRootAdmin",
     pwd: "password",
     roles: [ { role: "root", db: "admin" } ]
   });
> exit
bye
root@node1:/# exit

# step 4
root@node1:/# docker stop mongo
root@node1:/# docker rm mongo

# step 5
root@node1:/# docker run \
--name mongo \
-v /home/core/mongo-files/data:/data/db \
-v /home/core/mongo-files:/opt/keyfile \
--hostname="node1.example.com" \
--add-host node1.example.com:${node1} \
--add-host node2.example.com:${node2} \
--add-host node3.example.com:${node3} \
-p 27017:27017 -d mongo:2.6.5 \
--smallfiles \
--keyFile /opt/keyfile/mongodb-keyfile \
--replSet "rs0"

# step 6
root@node1:/# docker exec -it mongo /bin/bash
root@node1:/# mongo
MongoDB shell version: 2.6.5
> use admin
switched to db admin
> db.auth("siteRootAdmin", "password");
> rs.initiate()

# step 7
> rs.conf()

# step 8
#Perform on Node 2
root@node2:/# docker run \
--name mongo \
-v /home/core/mongo-files/data:/data/db \
-v /home/core/mongo-files:/opt/keyfile \
--hostname="node2.example.com" \
--add-host node1.example.com:${node1} \
--add-host node2.example.com:${node2} \
--add-host node3.example.com:${node3} \
-p 27017:27017 -d mongo:2.6.5 \
--smallfiles \
--keyFile /opt/keyfile/mongodb-keyfile \
--replSet "rs0"

# Perform on Node 3
root@node3:/# docker run \
--name mongo \
-v /home/core/mongo-files/data:/data/db \
-v /home/core/mongo-files:/opt/keyfile \
--hostname="node3.example.com" \
--add-host node1.example.com:${node1} \
--add-host node2.example.com:${node2} \
--add-host node3.example.com:${node3} \
-p 27017:27017 -d mongo:2.6.5 \
--smallfiles \
--keyFile /opt/keyfile/mongodb-keyfile \
--replSet "rs0"

# step 9
rs0:PRIMARY> rs.add("node2.example.com")
rs0:PRIMARY> rs.add("node3.example.com")
rs0:PRIMARY> rs.status()
```
copy from [Deploy a MongoDB Cluster in 9 steps Using Docker](https://medium.com/@gargar454/deploy-a-mongodb-cluster-in-steps-9-using-docker-49205e231319)
also see [
Enforce Keyfile Access Control in a Replica Set](https://docs.mongodb.com/manual/tutorial/enforce-keyfile-access-control-in-existing-replica-set/)

## installing mongodb 4.0

Debian

``` shell
sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 9DA31620334BD75D9DCB49F368818C72E52529D4
sudo echo "deb http://repo.mongodb.org/apt/debian stretch/mongodb-org/4.0 main" | sudo tee /etc/apt/sources.list.d/mongodb-org-4.0.list
sudo apt-get update
sudo apt-get install -y mongodb-org=4.0.0 mongodb-org-server=4.0.0 mongodb-org-shell=4.0.0 mongodb-org-mongos=4.0.0 mongodb-org-tools=4.0.0
```

CentOS

``` shell
vim /etc/yum.repos.d/mongodb-org-4.0.repo
```
use this below:

``` shell
[mongodb-org-4.0]
name=MongoDB Repository
baseurl=https://repo.mongodb.org/yum/redhat/$releasever/mongodb-org/4.0/x86_64/
gpgcheck=1
enabled=1
gpgkey=https://www.mongodb.org/static/pgp/server-4.0.asc
```

install:

``` shell
sudo yum install -y mongodb-org
```

## add username

``` shell
db.createUser({user: "username" ,
    pwd: passwordPrompt(),      // Or  "<cleartext password>"
    roles: [{role: "readWriteAnyDatabase", db: "admin"}, "readWrite"]})
```
copy from [Mongodb 用户权限管理及配置](https://blog.51cto.com/wzlinux/2153062)
also see[db.createUser()](https://docs.mongodb.com/manual/reference/method/db.createUser/)

## robo3t often need to connect to admin database, and then change the database.
