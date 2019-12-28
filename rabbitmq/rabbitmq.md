# rabbitmq

## start rabbitmq using docker

```
 docker run --hostname rabbitmq_test --name some-rabbit -p 5672:5672 -p 15672:15672 -e RABBITMQ_DEFAULT_USER=user -e RABBITMQ_DEFAULT_PASS=Anaith7x -e RABBITMQ_ERLANG_COOKIE='Chiew0irtoh9Peeg' -e RABBITMQ_DEFAULT_VHOST='abc' -d rabbitmq:3.8.2-management-alpine
```
