# docker example

``` shell
# daemon
docker run --detach --name web nginx:latest

# interactive
docker run --interactive --tty busybox:latest /bin/sh

# restart
docker restart web

# logs
docker logs web

# stop
docker stop web

# exec command
docker exec web ps

# rename
docker rename web web-old

```
