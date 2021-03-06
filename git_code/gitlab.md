# gitlab

## gitlab start as a docker container

``` shell
export VERSION=10.0.3-ce.0
docker pull gitlab/gitlab-ce:$VERSION
docker run -p 8030:80 -p 1443:443 -p 2222:22 \
    -d --name gitlab-ce \
    -v $HOME/app-conf/gitlab/etc:/etc/gitlab \
    -v $HOME/app-conf/gitlab/var/opt:/var/opt/gitlab \
    -v $HOME/app-conf/gitlab/log:/var/log/gitlab \
	--restart always \
    gitlab/gitlab-ce:$VERSION
```
see [使用 docker 安装 GitLab CE 和 GitLab CI](https://gist.github.com/kxxoling/dfa6659829934edc296a406e52f2d585)

## docker image update
Just rm the old container, and run the new version image. The project data will be used in the mounted dir.

``` shell
docker stop gitlab-ce
docker rm gitlab-ce
export VERSION=14  # for example new version is 11
docker pull gitlab/gitlab-ce:$VERSION
docker run -p 8030:80 -p 1443:443 -p 2222:22 \
    -d --name gitlab-ce \
    -v etc:/etc/gitlab \
    -v opt:/var/opt/gitlab \
    -v log:/var/log/gitlab \
	--restart always \
    gitlab/gitlab-ce:$VERSION
```

## set gitlab root password in version 14

``` shell
gitlab-rails console
irb(main):001:0>  u=User.where(id:1).first
1> u.password='Ia4eutee'
2> u.password_confirmation='Ia4eutee'
3> u.save!
```
