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
$docker commit -m "add file" -a "new img" 38d5616406a7 new_img_name

# tag and push
docker tag new_img_name:latest user/new_img_name:latest
docker push user/new_img_name:latest
```
