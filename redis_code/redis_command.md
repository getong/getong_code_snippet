# redis command

## nx option
nx (not exist) make the set key if exist failed.

``` shell
set key value nx
```

## bgsave costs a lot cpu, use it carefully.

## disable save
in redis.conf

```
#save 900 1
#save 300 10
#save 60 10000
```

Or

```
save ""
```
Then restart the redis process.

We can change it without restarting.

``` shell
$ redis-cli
1>CONFIG SET save ""
```

## redis on travis ci needs sudo
see [Redis fails to start on trusty beta image](https://github.com/travis-ci/travis-ci/issues/7941)
add this in .travis.yml

```
sudo: required
services:
  - redis-server
```

## order set

```
Range of integer scores that can be expressed precisely
Redis sorted sets use a double 64-bit floating point number to represent the score. In all the architectures we support, this is represented as an IEEE 754 floating point number, that is able to represent precisely integer numbers between -(2^53) and +(2^53) included. In more practical terms, all the integers between -9007199254740992 and 9007199254740992 are perfectly representable. Larger integers, or fractions, are internally represented in exponential form, so it is possible that you get only an approximation of the decimal number, or of the very big integer, that you set as score.
```
copy from [ZADD key](https://redis.io/commands/zadd)

```
 long 整数长度总共有 19位，923XXX.......，时间戳 毫秒精度 是 13位，所以只需 14 ~ 19 位存 等级，其他13位存时间。接下来看怎么存。

 等级偏移： Math.power(10, 14) = 10000000000000000（14位）

 这里有一个最大时间 MAX_TIME = 9999999999999 （13位）

 A 玩家，（10 * 等级偏移） + MAX_TIME - 11111111111111（ 时间戳），最终分数 10888888888888888
 B 玩家，（10 * 等级偏移） + MAX_TIME - 22222222222222（ 时间戳），最终分数 10777777777777777

 最终排序，A 玩家依然是第一。通过分数可以解析出真实 【等级 = 分数 / 等级偏移，取整】
 方式二 14 ~ 19位，那么等级最大数据就只能是 919999，超过这个数就会溢出。可以把时间戳降低到秒级别，可以支持更大数字
```
copy from [Redis 排行榜 相同分数根据时间优先排行](https://www.cnblogs.com/cci8go/p/5964485.html)

## redis connection string

``` shell
redis://arbitrary_usrname:password@ipaddress:6379/0
```
copy from [How to create a redis cloud connection url with an auth password](https://stackoverflow.com/questions/44344628/how-to-create-a-redis-cloud-connection-url-with-an-auth-password)

## redis username and password

redis.conf
```
user bert allcommands allkeys on >abc123
requirepass foobar
```
The 'user' command adds the user, and the requirepass command just sets the password for user 'default'.
redis-cli code:

``` shell
Redis# redis-cli -a foobar
Warning: Using a password with '-a' or '-u' option on the command line interface may not be safe.
127.0.0.1:6379> ACL LIST
1) "user bert on #6ca13d52ca70c883e0f0bb101e425a89e8624de51db2d2392593af6a84118090 ~* &* +@all"
2) "user default on #c3ab8ff13720e8ad9047dd39466b3c8974e592c2fa383d4a3960714caef0c4f2 ~* &* +@all"
```

or

``` shell
redis-cli --user bert --pass abc123
```
required redis 6.0 or above.
copy from [Does Redis use a username for authentication?](https://stackoverflow.com/questions/46569432/does-redis-use-a-username-for-authentication)
