# docker example

``` shell
# list all images
docker images

# pull
docker pull nginx

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

# save and load
docker save -o nginx.tar nginx
docker load --input nginx.tar

# commit
$ docker run -ti nginx /bin/bash
root@38d5616406a7:/# touch test
root@38d5616406a7:/# exit
$docker commit -m "commit msg" -a "auther@test.com" 38d5616406a7 new_img_name

# tag and push
docker tag new_img_name:latest user/new_img_name:latest
docker push user/new_img_name:latest

# bind mount volumn
docker run -d --name web -v ~/docs:/usr/local/apapche2/htdocs ubuntu

# mount managed volumn
docker run -d -v /var/lib/data --name web ubuntu

# romove all the containers
docker ps -a | awk '{print $1}' | sed '1d' | xargs -I xxx docker rm xxx

# remove all the images
docker images | awk '{print $3}' | sed '1d' | xargs -I xxx docker rmi -f xxx

# closed container
$cid=$(docker run -d --net none alpine:latest ip addr)
$docker logs $cid
1: lo: <LOOPBACK,UP,LOWER_UP> mtu 65536 qdisc noqueue state UNKNOWN
    link/loopback 00:00:00:00:00:00 brd 00:00:00:00:00:00
    inet 127.0.0.1/8 scope host lo
       valid_lft forever preferred_lft forever
    inet6 ::1/128 scope host
       valid_lft forever preferred_lft forever

# bridged container
$ docker run --rm --net bridge alpine:latest ip addr

# joined container
Share the container among the container network.
First container
$ docker run -d --name brady --network non alpine:latest nc -l 127.0.0.1:3333

The second container
$ docker run -it --network container:brady alpine:latest netstat -al

# open contrainer
docker run --rm --network host alpine:latest ip addr

# get the contaniner commit history
docker history container_id

# from dockerfile, the build command. The dockerfile is in current working directory.
docker build -t tag_name .

```

## entrypoint
entrypoint is a container loading and running command. If there are many entryponits, the last command will be running.
