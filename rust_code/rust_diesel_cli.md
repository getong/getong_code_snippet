# rust diesel_cli

## installation

``` shell
## debian
sudo apt-get install libsqlite3-dev postgresql-server-dev-11 postgresql-server-dev-all default-libmysqlclient-dev
cargo install diesel_cli

## MacOS
brew install postgresql mysql
```


## migration

``` shell
cargo new project_name
cd project_name
mkdir migrations
diesel migration generate table_name

## edit table_name files
```


## setup

``` shell
docker run --name postgres-db -e POSTGRES_PASSWORD=mypassword -p 15432:5432 -d postgres:13.1-alpine
cargo new --lib diesel_demo
cd diesel_demo
cargo add diesel dotenv
cargo build
echo DATABASE_URL=postgres://postgres:mypassword@localhost:15432/diesel_demo > .env
diesel setup
```
