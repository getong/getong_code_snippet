# clickhouse installation

## debian installation

``` shell
sudo apt-get install apt-transport-https ca-certificates dirmngr
sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv E0C56BD4

echo "deb https://repo.clickhouse.com/deb/stable/ main/" | sudo tee \
    /etc/apt/sources.list.d/clickhouse.list
sudo apt-get update

sudo apt-get install -y clickhouse-server clickhouse-client

sudo systemctl start clickhouse-server
```

## set the password of the default user

``` shell
echo -n $password | sha256sum
```
set the result to the `/etc/clickhouse-server/users.d/default-password.xml`

``` shell
sudo systemctl restart clickhouse-server
```

## allow remote access
edit file `/etc/clickhouse-server/config.xml`
uncomment `<listen_host>::</listen_host>`
``` shell
sudo systemctl restart clickhouse-server
```
