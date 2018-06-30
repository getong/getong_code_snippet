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
