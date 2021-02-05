# rust diesel_cli

## installation

``` shell
sudo apt-get install libsqlite3-dev postgresql-server-dev-11 postgresql-server-dev-all default-libmysqlclient-dev
cargo install diesel_cli
```


## migration

``` shell
cargo new project_name
cd project_name
mkdir migrations
diesel migration generate table_name

## edit table_name files
```
