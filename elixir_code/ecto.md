# ecto

## ecto works as a database middleware
The [getting started guide](https://hexdocs.pm/ecto/getting-started.html) is a must read.

## limit sql example

```
import Ecto.Query

def list_messages(room_id, limit \\ 15) do
  Repo.all(
    from msg in Message,
    join: user in assoc(msg, :user),
    where: msg.room_id == ^room_id,
    order_by: [desc: msg.inserted_at],
    limit: ^limit,
    select: %{content: msg.content, user: %{username: user.username}}
  )
end
```
copy from [More about Ecto and Ecto queries](http://whatdidilearn.info/2018/03/18/more-about-ecto-and-ecto-queries.html)

## multi primary keys

``` elixir
    alter table(:rooms_units) do
      modify(:room_id, :integer, primary_key: true)
      modify(:unit_id, :integer, primary_key: true)
      modify(:date_from, :utc_datetime, primary_key: true)
      modify(:date_to, :utc_datetime, primary_key: true)
    end
```
copy from [ecto中的复合主键](https://xbuba.com/questions/55038829)
elixir schema definition

``` elixir
  @primary_key false
  schema "players" do
    field :name, :string, primary_key: true
    field :position, :string, primary_key: true
    field :number, :integer

    timestamps
  end
```
copy from [Support for composite primary keys in schema](https://github.com/elixir-ecto/ecto/pull/1210)

## rename postgres table

``` elixir
qry = "ALTER TABLE abc RENAME to abc_20190516;"
Ecto.Adapters.SQL.query(Repo, qry, [])
```
copy from [How to use raw sql with ecto Repo](https://stackoverflow.com/questions/27751216/how-to-use-raw-sql-with-ecto-repo)

## Elixir Ecto: 多对多关系
read the doc [Elixir Ecto: 多对多关系](https://segmentfault.com/a/1190000005036198)

## Ecto query pipeline

``` elixir
def getProductByNameAndBrand(name, brand) do
    Api.Product |> Ecto.Query.where(name: ^name) |> Ecto.Query.where(brand: ^brand) |> all
  end
```
copy from [Ecto 'where and where' clause](https://stackoverflow.com/questions/44211863/ecto-where-and-where-clause)

``` elixir
"users"
|> where([u], u.age > 18)
|> select([u], u.name)
```
copy from [Ecto.Query](https://hexdocs.pm/ecto/Ecto.Query.html)
