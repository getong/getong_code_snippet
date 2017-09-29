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
