# phoenix example

## install inotify-tools

``` shell
sudo apt-get install -y inotify-tools
```

## running postgres with docker

``` shell
docker run --name postgres_instance -e TZ=Asia/Shanghai -e POSTGRES_USER=user_name -e POSTGRES_PASSWORD=aek4iTu6 -e POSTGRES_DB=db_name -d -p 15432:5432 postgres:11.3
```
psql connection:

``` shell
 PGPASSWORD=aek4iTu6 psql -h localhost -U user_name  -p 15432 db_name
```

## phoenix example

``` shell
## install hex
$ mix local.hex --force

## install phoenix framework
$ mix archive.install hex phx_new 1.4.7 --force

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

## Building a JSON API in Elixir with Phoenix 1.4
A very good example for JSON API on [Building a JSON API in Elixir with Phoenix 1.4](https://lobotuerto.com/blog/building-a-json-api-in-elixir-with-phoenix/)

## make resource paths with a different key than :id
The default key in the path is :id, and the :id is the default key autogenerate in the table.
But some time, the default key is not the :id in the table definition. So we need to set a different key in the resource.
According to [Ecto+Phoenix: How to make resource paths with a different key than :id?](https://stackoverflow.com/questions/37512534/ectophoenix-how-to-make-resource-paths-with-a-different-key-than-id), the right method is to `to_param`.

``` elixir
@derive {Phoenix.Param, key: :username}
schema "users" do
```
Copy from [Phoenix.Param](https://hexdocs.pm/phoenix/Phoenix.Param.html#to_param/1)

## Unique Identifier For Session
copy from [Unique Identifier For Session](https://elixirforum.com/t/unique-identifier-for-session/13778)
``` elixir
## add this into mix.exs
  defp deps do
    [
    ...
      {:elixir_uuid, "~> 1.2"}
    ]
  end

## create_token and validate_token
def create_token() do
  uuid = UUID.generate_v4() # you probably need to get a UUID package, therefore this is pseudo code
  Phoenix.Token.sign(YourApp.Endpoint, "your secret salt", uuid)
end

def validate_token(token) do
  Phoenix.Token.verify(YourApp.Endpoint, "your secret salt", token, max_age: 5 * 60)
end
```

## cookies

```
`conn.cookies` the request cookies with the response cookies
`conn.req_cookies` the request cookies (without the response ones), of course there is `coon.resp_cookies` for the response cookies.
```
copy from [Elixir/Phoenix - Accessing user's cookie: conn.cookies vs conn.req_cookies vs conn.req_headers](https://stackoverflow.com/questions/51075838/elixir-phoenix-accessing-users-cookie-conn-cookies-vs-conn-req-cookies-vs-co)
