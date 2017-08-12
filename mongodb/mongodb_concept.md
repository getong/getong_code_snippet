# mongodb concept

## 文档是MongoDB中数据的基本单元， 非常类似于关系型数据库管理系统中的行， 但更有表现力。

## 类似地，集合(collection)可以看作是一个拥有动态模式(dynamic schema)的表。

## MongoDB的一个实例可以拥有多个相互独立的数据库(database), 每一个数据库都拥有自己的集合。

## 每一个文档都有一个特殊的键"_id", 这个键在文档所属的集合中是唯一的。

## 对应函数
辅助函数 | 等价函数
------------ | -------------
use foo | db.getSisterDB("foo")
show dbs | db.getMongo().getDBs()
show collections | db.getCollectionNames()
