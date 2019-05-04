# phoenix example

## install inotify-tools

``` shell
sudo apt-get install -y inotify-tools
```

## running postgres with docker

``` shell
docker run --name postgres_instance -e POSTGRES_PASSWORD=aek4iTu6 -d -p 15432:5432 postgres:11.2-alpine
```
psql connection:

``` shell
 PGPASSWORD=aek4iTu6 psql -h localhost -U postgres  -p 15432
```

## phoenix example

``` shell
## install hex
$ mix local.hex --force

## install phoenix framework
$ mix archive.install hex phx_new 1.4.4 --force

## phoenix hello project, when installing dependencies choose yes
$ mix phx.new hello

## with --no-ecto option
$ mix phx.new hello --no-ecto

## If in the last step, you choose no, you can do this manually
$ cd hello
$ mix deps.get
$ cd assets && npm install && node node_modules/webpack/bin/webpack.js --mode development

## cd hello
$ cd hello
```
edit the config/dev.exs like below:

```
config :hello, Hello.Repo,
  username: "postgres",
  password: "aek4iTu6",
  database: "postgres",
  hostname: "localhost",
  port: 15432,
  pool_size: 10
```
Then continue:

``` shell
## Start your Phoenix app with:
$ mix phx.server
## You can also run your app inside IEx (Interactive Elixir) as:
$ iex -S mix phx.server
```

## learn phoenix framework
[Learn Phoenix (Web App) Framework](https://github.com/dwyl/learn-phoenix-framework)

## postgres dump db struct

export db struct
``` shell
PGPASSWORD=abc123 pg_dump -h localhost -U user -C -p 5432 -s db -f db_struct.sql
```
import db struct

``` shell
PGPASSWORD=abc123 psql -h localhost -U user -p 5432 -f db_struct.sql
```
