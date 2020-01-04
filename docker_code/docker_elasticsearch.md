# docker elasticsearch

## pull the tag

```
docker pull elasticsearch:7.5.0
```

## run the elasticsearch:

```
docker network create somenetwork
docker run -d --name elasticsearch --net somenetwork -p 9200:9200 -p 9300:9300 -e "discovery.type=single-node" elasticsearch:7.5.1
```

## elasticsearch and kibana

see [elasticsearch.md](https://github.com/jaywcjlove/docker-tutorial/blob/master/docker/elasticsearch.md)
[docker安装elasticsearch](https://juejin.im/post/5ca0d12c518825550b35be6d)
