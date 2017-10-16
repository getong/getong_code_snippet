# gitlab

## gitlab start as a docker container

``` shell
export VERSION=10.0.3-ce.0
docker pull gitlab/gitlab-ce:$VERSION
docker run -p 8030:80 -d --name gitlab-ce \
    -v $HOME/app-conf/gitlab/etc:/etc/gitlab \
    -v $HOME/app-conf/gitlab/var/opt:/var/opt/gitlab \
    -v $HOME/app-conf/gitlab/log:/var/log/gitlab \
	--restart always \
    gitlab/gitlab-ce:$VERSION
```

## docker image update
Just rm the old container, and run the new version image. The project data will be used in the mounted dir.

``` shell
docker stop gitlab-ce
docker rm gitlab-ce
export VERSION=11  # for example new version is 11
docker pull gitlab/gitlab-ce:$VERSION
docker run -p 8030:80 -d --name gitlab-ce \
    -v $HOME/app-conf/gitlab/etc:/etc/gitlab \
    -v $HOME/app-conf/gitlab/var/opt:/var/opt/gitlab \
    -v $HOME/app-conf/gitlab/log:/var/log/gitlab \
	--restart always \
    gitlab/gitlab-ce:$VERSION
```
