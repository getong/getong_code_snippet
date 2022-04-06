# docker mariadb

See the reference [使用dokcer 构建 mariadb 数据库](http://dockone.io/article/2380)

## setup the mariadb container

``` shell
mkdir -p /data/mysql/data
chcon -Rt svirt_sandbox_file_t /data/mysql/data
podman run -v /data/mysql/data:/var/lib/mysql -p 4444:3306 -e MYSQL_DATABASE=test_db -e MYSQL_USER=user_a -e MYSQL_ROOT_PASSWORD=zan3Kie1 --name mariadb_instance -d mariadb:10.4.11 --character-set-server=utf8mb4 --collation-server=utf8mb4_general_ci

## docker run --privileged -v /data/mysql/data:/var/lib/mysql -p 4444:3306 -e MYSQL_DATABASE=test_db -e MYSQL_USER=user_a -e MYSQL_ROOT_PASSWORD=zan3Kie1 --name mariadb_instance -d mariadb:10.4.11 --character-set-server=utf8mb4 --collation-server=utf8mb4_general_ci

mkdir -p data
chcon -Rt svirt_sandbox_file_t /data/mysql/data

podman run -v "$PWD/data":/var/lib/mysql -p 4444:3306 -e MYSQL_DATABASE=test_db -e MYSQL_USER=user_a -e MYSQL_ROOT_PASSWORD=zan3Kie1 --name mariadb_instance -d mariadb:10.4.11 --character-set-server=utf8mb4 --collation-server=utf8mb4_general_ci

# docker run --privileged -v "$PWD/data":/var/lib/mysql -p 4444:3306 -e MYSQL_DATABASE=test_db -e MYSQL_USER=user_a -e MYSQL_ROOT_PASSWORD=zan3Kie1 --name mariadb_instance -d mariadb:10.4.11 --character-set-server=utf8mb4 --collation-server=utf8mb4_general_ci

```
If not root user, it must uses `--privileged` parameter to obtain root privilege.

## use the mariadb

``` shell
docker exec -it mariadb bash
root@XXXX:/# mysql -uroot -proot
Welcome to the MariaDB monitor. Commands end with ; or \g.
Your MariaDB connection id is 3
Server version: 10.1.19-MariaDB-1~jessie mariadb.org binary distribution

Copyright (c) 2000, 2016, Oracle, MariaDB Corporation Ab and others.

Type 'help;' or '\h' for help. Type '\c' to clear the current input statement.

MariaDB [(none)]> show databases;
```

## use the mysql client

``` shell
mysql -uroot -p -h127.0.0.1 -P 4444
```
It must use `-h` parameter.

## start phpmyadmin, do not use the fpm version

``` shell

podman run --name my-own-mariadb -p 3306:3306 -e MYSQL_ROOT_PASSWORD=123456 -d mariadb:10.4.10
podman run --name my-own-phpmyadmin -d --link my-own-mariadb:db -p 8080:80 phpmyadmin/phpmyadmin:4.9

# docker run --name my-own-mariadb -p 3306:3306 -e MYSQL_ROOT_PASSWORD=123456 -d mariadb:10.4.10
# docker run --name my-own-phpmyadmin -d --link my-own-mariadb:db -p 8080:80 phpmyadmin/phpmyadmin:4.9
curl http://127.0.0.1:8080
```
read from [Run MySQL & phpMyAdmin locally in 3 steps using Docker](https://medium.com/@migueldoctor/run-mysql-phpmyadmin-locally-in-3-steps-using-docker-74eb735fa1fc)

## phpmyadmin connect to remote server

``` shell
podman run --name other_phpmyadmin -d -e PMA_HOST=remote_host_or_ip -e PMA_PORT=remote_port -e MYSQL_USER=remote_user -e MYSQL_PASSWORD=remote_password  -p 8888:80 phpmyadmin/phpmyadmin:5.0.2
# docker run --privileged --restart=always --name other_phpmyadmin -d -e PMA_HOST=remote_host_or_ip -e PMA_PORT=remote_port -e MYSQL_USER=remote_user -e MYSQL_PASSWORD=remote_password  -p 8888:80 phpmyadmin/phpmyadmin:5.0.2
```

## mount conf directory

``` shell
podman run -p 3306:3306 --name mysql -v $PWD/conf:/etc/mysql/conf.d -v $PWD/logs:/logs -v $PWD/data:/var/lib/mysql -e MYSQL_ROOT_PASSWORD=123456 -d mysql:8.0

# docker run -p 3306:3306 --name mysql -v $PWD/conf:/etc/mysql/conf.d -v $PWD/logs:/logs -v $PWD/data:/var/lib/mysql -e MYSQL_ROOT_PASSWORD=123456 -d mysql:8.0

```
The meaning is:

```
-p 3306:3306：将容器的 3306 端口映射到主机的 3306 端口。
-v -v $PWD/conf:/etc/mysql/conf.d：将主机当前目录下的 conf/my.cnf 挂载到容器的 /etc/mysql/my.cnf。
-v $PWD/logs:/logs：将主机当前目录下的 logs 目录挂载到容器的 /logs。
-v $PWD/data:/var/lib/mysql ：将主机当前目录下的data目录挂载到容器的 /var/lib/mysql 。
-e MYSQL_ROOT_PASSWORD=123456：初始化 root 用户的密码。
```
copy from [docker安装mysql，并修改my.cnf文件](https://blog.csdn.net/qq_29907885/article/details/107692317)
