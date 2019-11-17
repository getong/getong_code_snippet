# phoenix example

## install inotify-tools

``` shell
sudo apt-get install -y inotify-tools
```

## running postgres with docker

``` shell
docker run --name postgres_instance -e TZ=Asia/Shanghai -e POSTGRES_USER=user_name -e POSTGRES_PASSWORD=aek4iTu6 -e POSTGRES_DB=db_name -d -p 15432:5432 postgres:12.0
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
$ mix archive.install hex phx_new 1.4.10 --force

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

## configure the option

``` elixir
config :example, Example.Endpoint,
  http: [
    port: 4000,
    transport_options: [
      num_acceptors: 50, ## default is 100
      max_connections: 8888 ## default is 16_384
    ]
  ],
```
see the [Plug.Cowboy](https://hexdocs.pm/plug_cowboy/Plug.Cowboy.html)

## phoenix_live_view
[Walk-Through of Phoenix LiveView](https://elixirschool.com/blog/phoenix-live-view)

## gen.secret

``` shell
mix phx.gen.secret
```

## guardian
[guardian](https://elixirschool.com/zh-hans/lessons/libraries/guardian/)

## websocket
[phoenix_chat_example](https://github.com/chrismccord/phoenix_chat_example)
[Elixir WebSocket Chat Example w/o Phoenix](https://medium.com/@loganbbres/elixir-websocket-chat-example-c72986ab5778)

## plug parsers
Plug only have default parsers to json, multipart and urlencoded.
see [Only allowing XML requests](https://elixirforum.com/t/only-allowing-xml-requests/5153/2)

## Bcrypt
add Bcrypt in the mix.ex
``` shell
{:bcrypt_elixir, "~> 2.0"}
```
basic usage:

``` shell
salt = Bcrypt.gen_salt(5)
hash = Bcrypt.Base.hash_password(password = "test_password", salt)
Bcrypt.verify_pass(password = "test_password", hash)

## belongs_to
gen the schema
``` shell
mix phx.gen.schema UserServer user_server body:text video_id:references:videos
```
in the schema

``` elixir
schema user_server do
field :body, :string
belogs_to :video, Video
end
```

## scrivener_ecto

in the mix.exs file:

``` elixir
{:scrivener_ecto, "~> 2.2"}
```

in the Repo.ex

``` elixir
defmodule MyApp.Repo do
  use Ecto.Repo, otp_app: :my_app, adapter: Ecto.Adapters.Postgres
  use Scrivener, page_size: 10
end
```

usage:

``` elixir
Schema |> Imcircle.Repo.paginate(page: 1)
```

## view and template

```
it’s worth emphasizing that a view in Phoenix is just a module, and templates are just functions.
```

## file upload
[File Uploads](https://phoenixframework.org/blog/file-uploads)
[Elixir / Phoenix — Uploading images locally (With ARC)](https://medium.com/@Stephanbv/elixir-phoenix-uploading-images-locally-with-arc-b1d5ec88f7a)

``` elixir
<%= form_for @conn, Routes.page_path(:conn, :upload_img), [multipart: true], fn f -> %>
  <div class="form-group">
    <label>Photo</label>
    <%= file_input f, :photo, class: "form-control" %>
  </div>

  <div class="form-group">
    <%= submit "Submit", class: "btn btn-primary" %>
  </div>
<% end %>
```

in the controller:

``` elixir
def create(conn, %{"user" => user_params}) do
    IO.inspect user_params
    if upload = user_params["photo"] do
        extension = Path.extname(upload.filename)
        File.cp(upload.path, "/media/#{user.id}-profile#{extension}")
    end
    ...
end
```

## show images in the browser
[Creating a Photo Gallery in Phoenix with Arc - Part 1](https://experimentingwithcode.com/creating-a-photo-gallery-in-phoenix-with-arc-part-1/)

``` elixir
plug Plug.Static,
  at: "/uploads",
  # from: Path.expand("./uploads"),
  from: "./uploads",
  gzip: false
```

## enable websocket
edit the endpoint.ex

``` elixir
socket "/socket", Web.UserSocket,
websocket: true,
longpoll: false
```
edit usersocket.ex
```
channel "room:*", Web.RoomChannel
```

## How To Get Phoenix & VueJS working Together
[How To Get Phoenix & VueJS working Together](https://elixirforum.com/t/how-to-get-phoenix-vuejs-working-together/5108)

## channel detail
the default protocol is websocket, but the data transport is serialized by its serializer.ex module, see phoenix/lib/phoenix/socket/serializer.ex and also phoenix/lib/phoenix/socket/serializers, currently it has two implementations: v1_json_serializer.ex and v2_json_serializer.ex.
And its data form is just like:

``` json
{
  "topic": "...",
  "event": "...",
  "payload": {},
  "ref": 0
}
```
these four fields are all needed.
[Websocket Clients and Phoenix Channels](http://graemehill.ca/websocket-clients-and-phoenix-channels/) is a must read.

## use wscat to connect phoenix channel

``` shell
sudo npm install -g wscat
wscat -c "ws://localhost:4000/socket/websocket"
```
then type these in the open terminal

``` json
{  "topic": "room:lobby",  "event": "phx_join",  "payload": {},  "ref": 0}
{  "topic": "room:lobby",  "event": "heartbeat",  "payload": {},  "ref": 0}
{  "topic": "room:lobby",  "event": "echo",  "payload": { "hello": "world" },  "ref": 0}
```
all are copied from [Websocket Clients and Phoenix Channels](http://graemehill.ca/websocket-clients-and-phoenix-channels/).
[Use wscat to Connect to a WebSocket API and Send Messages to It](https://docs.aws.amazon.com/apigateway/latest/developerguide/apigateway-how-to-call-websocket-api-wscat.html)

## phoenix channel ip

``` elixir
  socket("/socket", MyApp.Web.UserSocket,
    websocket: [connect_info: [:peer_data, :x_headers]],
    longpoll: [connect_info: [:peer_data, :x_headers]]
  )

  info: %{
  peer_data: %{address: {127, 0, 0, 1}, port: 52372, ssl_cert: nil},
  x_headers: []
}
```
copy from [Phoenix socket/channels security / IP identification](https://elixirforum.com/t/phoenix-socket-channels-security-ip-identification/1463)

also see [Endpoint](https://hexdocs.pm/phoenix/Phoenix.Endpoint.html#socket/3)

## layout
``` elixir
layout(conn)
false

conn = put_layout conn, {AppView, "application.html"}
layout(conn)
{AppView, "application.html"}

conn = put_layout conn, "print.html"
layout(conn)
{AppView, "print.html"}

conn = put_layout conn, :print
layout(conn)
{AppView, :print}
```
## CorsPlug
see [CorsPlug](https://hexdocs.pm/cors_plug/readme.html#content)

## priv repo migration file name

``` elixir
@doc false
  def copy_new_files(%Schema{context_app: ctx_app} = schema, paths, binding) do
    files = files_to_be_generated(schema)
    Mix.Phoenix.copy_from(paths, "priv/templates/phx.gen.schema", binding, files)

    if schema.migration? do
      migration_path = Mix.Phoenix.context_app_path(ctx_app, "priv/repo/migrations/#{timestamp()}_create_#{schema.table}.exs")
      Mix.Phoenix.copy_from paths, "priv/templates/phx.gen.schema", binding, [
        {:eex, "migration.exs", migration_path},
      ]
    end

    schema
  end

defp timestamp do
    {{y, m, d}, {hh, mm, ss}} = :calendar.universal_time()
    "#{y}#{pad(m)}#{pad(d)}#{pad(hh)}#{pad(mm)}#{pad(ss)}"
  end
  defp pad(i) when i < 10, do: << ?0, ?0 + i >>
  defp pad(i), do: to_string(i)
```

## cors

``` elixir
{:cors_plug, "~> 2.0"}
```
