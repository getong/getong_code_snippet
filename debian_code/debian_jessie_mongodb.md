# debian jessie mongodb

``` shell
$ sudo echo "deb http://repo.mongodb.org/apt/debian jessie/mongodb-org/3.4 main" |  tee /etc/apt/sources.list.d/mongodb-org-3.4.list
$ sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 0C49F3730359A14518585931BC711F9BA15703C6
$ sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv EF0F382A1A7B6500
$ sudo apt-get update
$ sudo apt-get install -y mongodb-org
$ sudo mkdir -p /data/db
$ sudo service mongod start
```
