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
## save the image
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

# use -h to add hostname
$ docker run -it --rm -h alpine_host alpine:latest /bin/sh
/ # hostname
alpine_host
/ # cat /etc/hosts
127.0.0.1       localhost
::1     localhost ip6-localhost ip6-loopback
fe00::0 ip6-localnet
ff00::0 ip6-mcastprefix
ff02::1 ip6-allnodes
ff02::2 ip6-allrouters
172.17.0.2      alpine_host
/ # ip a
1: lo: <LOOPBACK,UP,LOWER_UP> mtu 65536 qdisc noqueue state UNKNOWN
    link/loopback 00:00:00:00:00:00 brd 00:00:00:00:00:00
    inet 127.0.0.1/8 scope host lo
       valid_lft forever preferred_lft forever
9: eth0: <BROADCAST,MULTICAST,UP,LOWER_UP> mtu 1500 qdisc noqueue state UP
    link/ether 02:42:ac:11:00:02 brd ff:ff:ff:ff:ff:ff
    inet 172.17.0.2/16 scope global eth0
       valid_lft forever preferred_lft forever


# get the contaniner commit history
docker history container_id

# from dockerfile, the build command. The dockerfile is in current working directory.
docker build -t tag_name .
docker build -t tag_name -f dockerfile .

# export and import
# Note that, export is very likely with the save, but it removes the commit history.
# export the container
docker export --output new_image.tar container_id
docker import new_image.tar new_images:tag_name
docker import http://example.com/exampleimage.tgz img_name:tag_name

```

## entrypoint
entrypoint is a container loading and running command. If there are many entryponits, the last command will be running.

## get all tags of image on remote repo

``` shell
 wget -q https://registry.hub.docker.com/v1/repositories/debian/tags -O -  | sed -e 's/[][]//g' -e 's/"//g' -e 's/ //g' | tr '}' '\n'  | awk -F: '{print $3}'
```
see the [docker get list of tags in repository](https://fordodone.com/2015/10/02/docker-get-list-of-tags-in-repository/)

## inspect. Find out the meta data of an image or a container

``` shell
docker inspect image_name
docker inspect container_id
```

## attach. Attatch to the container stdin

``` shell
docker attach container_id
```

## exec. Running cmd in a container like pipeline

``` shell
docker exec -it container_id /bin/sh
docker exec -d container_id cmd
echo $?
```

## attch 后，如果从stdin中exit，会导致容器的停止， exec 不会
see [Docker exec与Docker attach](http://blog.csdn.net/halcyonbaby/article/details/46884605)


## cp
``` shell
docker cp docker_id:file file
docker cp file docker:file
```

## many parameters together

``` shell
docker run -d --privileged --restart=always --network host -v $(pwd):/mount_point -e ENV="env"  --name container_name image_name:tag_name
```
use host network for performance issue.

## No such file or directory for /var/lib/docker/overlay2

``` shell
sudo umount /var/lib/docker/overlay2
sudo rm -rf /var/lib/docker
sudo systemctl restart docker
```
see [No such file or directory for /var/lib/docker/overlay2 #1396](https://github.com/docker/for-mac/issues/1396)

## -q cmd option

``` shell
-q, --quiet           Only show numeric IDs
```

## delete no tag name images

``` shell
docker rmi $(docker images -q --filter "dangling=true")
```

## delete all Exited contrainers

``` shell
docker rm $(docker ps -a | grep Exited | awk '{print $1}')
```

## stop and delete all containers

``` shell
docker rm $(docker ps -a -q)
```

## delete all images

``` shell
docker rmi $(docker images -a -q)
```

## start the stopped container

``` shell
docker start container_id
```
