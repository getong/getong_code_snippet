# dockerfile

## cmd
> CMD在Dockerfile文件中仅可指定一次，指定多次时，会覆盖前的指令。
> 另外，docker run命令也会覆盖Dockerfile中CMD命令。
> 如果docker run运行容器时，使用了Dockerfile中CMD相同的命令，就会覆盖Dockerfile中的CMD命令。
> 在使用docker run运行容器时，我们并没有在命令结尾指定会在容器中执行的命令，这时Docker就会执行在Dockerfile的CMD中指定的命令。

> 如果不想使用CMD中指定的命令，就可以在docker run命令的结尾指定所要运行的命令：
see [CMD](https://itbilu.com/linux/docker/VyhM5wPuz.html#cmd-cmd)

## entrypoint
> ENTRYPOINT与CMD非常类似，不同的是通过docker run执行的命令不会覆盖ENTRYPOINT，而docker run命令中指定的任何参数，都会被当做参数再次传递给ENTRYPOINT。
> Dockerfile中只允许有一个ENTRYPOINT命令，多指定时会覆盖前面的设置，而只执行最后的ENTRYPOINT指令。

> docker run运行容器时指定的参数都会被传递给ENTRYPOINT，且会覆盖CMD命令指定的参数。
> 如，执行docker run <image> -d时，-d参数将被传递给入口点。

> 也可以通过docker run --entrypoint重写ENTRYPOINT入口点。
see [ENTRYPOINT](https://itbilu.com/linux/docker/VyhM5wPuz.html#cmd-entrypoint)


## timezone
Add this into dockerfile

``` shell
ENV TZ=Asia/Shanghai
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone
```
see [Docker Container time & timezone (will not reflect changes)](https://serverfault.com/questions/683605/docker-container-time-timezone-will-not-reflect-changes)

## --from directive

``` shell
FROM elixir:alpine
ARG project_id
...
FROM alpine:latest
COPY --from=0 /opt/release .
```
build command:

``` shell
docker build -t abc -f dockerfile . --build-arg project_id=abc
```

copy from [Run an Elixir Phoenix app in containers using Google Kubernetes Engine](https://cloud.google.com/community/tutorials/elixir-phoenix-on-kubernetes-google-container-engine)
