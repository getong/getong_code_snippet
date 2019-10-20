# ecto

## ecto works as a database middleware
The [getting started guide](https://hexdocs.pm/ecto/getting-started.html) is a must read.

## limit sql example

```
import Ecto.Query

def list_messages(room_id, limit \\ 15) do
  Repo.all(
    from msg in Message,
    left_join: user in assoc(msg, :user),
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

## join

```
Receives a source that is to be joined to the query and a condition for the join. The join condition can be any expression that evaluates to a boolean value. The join is by default an inner join, the qualifier can be changed by giving the atoms: :inner, :left, :right, :cross, :full, :inner_lateral or :left_lateral. For a keyword query the :join keyword can be changed to: :inner_join, :left_join, :right_join, :cross_join, :full_join, :inner_lateral_join or :left_lateral_join.
```
The use the :left_join is very common.
see [图解 SQL 里的各种 JOIN](https://mazhuang.org/2017/09/11/joins-in-sql/)
[Visual Representation of SQL Joins](https://www.codeproject.com/Articles/33052/Visual-Representation-of-SQL-Joins)

## transaction
The first way to run Repo.transaction is by passing in a function containing the operations you'd like to run within transaction.

``` elixir
Repo.transaction(fn ->
    Repo.insert(variable)
end)
```
rollback

``` elixir
Repo.transaction(fn ->
    case Repo.insert(variable) do
      {:ok, _variable} ->
          :ok
      {:error, _error_reason} ->
         Repo.rollback("insert failed")
    end
end)
```

The second way is to use Multi module.

``` elixir
multi =
    Multi.new()
    |> Multi.insert(:a, a)
    |> Multi.update(:b, b)
    |> Multi.run(:c, M, :function, args)

Repo.transaction(multi)
```

explain multi

``` elixir
Multi.to_list(multi)
```

## convert the ecto statement to sql

``` elixir
query = from Table, select: [:id]
Ecto.Adapters.SQL.to_sql(:all, Repo, query)

Repo.to_sql(:all, query)

IO.puts(elem(Imcircle.Repo.to_sql(:all, query), 0))
```

## Upserts

``` elixir
{:ok, ignored} = MyRepo.insert(%Post{title: "this is unique"}, on_conflict: :nothing)
on_conflict = [set: [body: "updated"]]
{:ok, updated} = MyRepo.insert(%Post{title: "this is unique"},
                               on_conflict: on_conflict, conflict_target: :title)

```
or use the query paramter:

``` elixir
query =
      Query.from(post in Post,
        where: post.number == 1,
        update: [set: [number: post.number + 1]]
      )
Repo.insert(%Post{number: 1},
      on_conflict: query,
      conflict_target: :number
    )
```

## bigint type

``` shell
mix phx.gen.context Context Schema table_name type:integer
```
then in the migration file, change the `integer` to `bigint`
befor:

``` elixir
add :type, :integer
```
after:

``` elixir
add :type, :bigint
```

and change the schema file:

``` elixir
field :type, :id
```

then run the migrate command:

``` shell
mix ecto.migrate
```
the default `id` type in the ecto is :bigint
see [:bigint field type](https://github.com/elixir-ecto/postgrex/issues/316)

## post schema
Writing a Custom Migration Task with Ecto Migrator
``` shell
defmodule MyApp.ReleaseTasks do
  def migrate do
   {:ok, _} = Application.ensure_all_started(:my_app)
    path = Application.app_dir(:my_app, "priv/repo/migrations")
    Ecto.Migrator.run(MyApp.Repo, path, :up, all: true)
  end
end
```
copy from [Run Ecto Migrations in Production with Distillery Boot Hooks](https://medium.com/flatiron-labs/run-ecto-migrations-in-production-with-distillery-boot-hooks-7f576d2b93ed)
user `prefix` option to indicate the postgresql schema.

create a new schema:

``` elixir
Ecto.Adapters.SQL.query(MyApp.Repo, "create schema schema_name")
```
## use variable in ecto query prefix

``` elixir
query =
        from u in User,
          order_by: [asc: u.full_name, asc: u.id],
          select: struct(u, [:id, :first_name, :last_name, :full_name])
query = Map.put(query, :prefix, params["prefix"])
Repo.all(query)
```
copy from [Using variable in ecto query prefix](https://elixirforum.com/t/using-variable-in-ecto-query-prefix/21178/4)

## Multi work with pipeline

work with the previous operation result
``` elixir
Ecto.Multi.new()
|> Ecto.Multi.insert(:team, team_changeset)
|> Ecto.Multi.run(:user, fn repo, %{team: team} ->
  # Use the inserted team.
  repo.update(user_changeset)
end)

Ecto.Multi.new()
|> Ecto.Multi.insert_all(:users, MyApp.User, users)
|> Ecto.Multi.run(:pro_users, fn _repo, %{users: users} ->
  result = Enum.filter(users, &(&1.role == "pro"))
  {:ok, result}
end)
```

work with multiple Multis and dynanic data:

``` elixir
posts_multi =
  posts
  |> Stream.filter(fn post ->
    # Filter old posts...
  end)
  |> Stream.map(fn post ->
    # Create changesets.
    Ecto.Changeset.change(post, %{category: "new"})
  end)
  |> Stream.map(fn post_cs ->
    # Create a Multi with a single update
    # operation, generating a unique key for the op.
    key = String.to_atom("post_#{post_cs.data.id})
    Ecto.Multi.update(Ecto.Multi.new(), key, post_cs)
  end)
  |> Enum.reduce(Multi.new(), &Multi.append/2)
```
copy from [A brief guide to Ecto.Multi](https://medium.com/heresy-dev/a-brief-guide-to-ecto-multi-9c8ea0c729f0)

## insert_all act like upsert_all

``` elixir
list_list =
a_very_long_list
|> Stream.map(fn _id ->
## here might be a map, not the schema struct
%{a: "a",
b: "b",
c: "c"
} end)
|> Stream.chunk_every(5000)
|> Enum.to_list()

Enum.each(list_list, fn list ->
      Repo.insert_all(SchemaTable, list)
    end)
```
The SchemaTable property list must all be set in the map list without the id property.

## insert act like upsert

``` elixir
data = %Data{}
Repo.insert(data, conflict_target: unchange_list, on_conflict: {:replace, change_list})
```

## dynamic table name
copy from [How can you dynamically set a schema table name for a ecto model](https://stackoverflow.com/questions/40687186/how-can-you-dynamically-set-a-schema-table-name-for-a-ecto-model)

``` elixir
## query
from p in {"posts2", Post}, where p.id == 1

## insert or update
Ecto.put_meta(struct, source: "source")
```

## Date/time intevals
Date/time intervals: datetime_add/3, date_add/3, from_now/2, ago/2
copy from [Ecto.Query.API](https://hexdocs.pm/ecto/Ecto.Query.API.html)

``` elixir
# Get all items published since the last month
## The following intervals are supported: year, month, week, day, hour, minute, second, millisecond and microsecond.
from p in Post, where: p.published_at >
                       datetime_add(^NaiveDateTime.utc_now, -1, "month")
# Get all items published since the last month
The following intervals are supported: year, month, week, day, hour, minute, second, millisecond and microsecond.
from p in Post, where: p.published_at >
                       datetime_add(^NaiveDateTime.utc_now, -1, "month")
from a in Account, where: a.expires_at < from_now(3, "month")
from p in Post, where: p.published_at > ago(3, "month")
```

## implement the Access behaviour for the Ecto.Schema file
put the below code into the schema file
``` elixir
@behaviour Access

defdelegate fetch(map, key), to: Map

defdelegate get_and_update(map, key, fun), to: Map

defdelegate pop(map, key), to: Map
```
As we all know, the schema is a map in elixir, and the Access behaviour needs to implement three callbacks:

``` elixir
fetch(term, key)
get_and_update(data, key, function)
pop(data, key)
```
here I just delegate the three functiosn to the Map module, and we can use Schema like this:

``` elixir
a = %Schema{}
a["property_a"]
```

## select_merge

``` elixir
 def for_admission(query \\ AdmissionEvent, admission) do
    from(ae in query,
      where: ae.admission_id == ^admission.id,
      order_by: [desc: ae.occurred_at],

      #### STEP TWO ####
      #  Join on User  #
      ##################
      join: u in User,
      on: ae.admitter_uuid == u.uuid,

      ############ STEP THREE #############
      #  Select Merge into Virtual Field  #
      #####################################
      select_merge: %{admitter_name: u.full_name}
    )
  end
end
```
the output:

``` elixir
iex(1)> admission = Repo.get(Admission, 1)
iex(2)> AdmissionEvent.for_admission(admission) |> Repo.all()
[
  %Registrar.Tracking.AdmissionEvent{
    __meta__: #Ecto.Schema.Metadata<:loaded, "admission_events">,
    action: "Student Admitted",
    admission_id: 3,
    admitter_name: "Albus Dumbledore",
    id: 1,
    occurred_at: ~N[2019-07-29 02:22:18]
  }
]
iex(3)> event = List.first(events)
iex(4)> event.admitter_name
"Albus Dumbledore"
```
copy from [TIL How to Select Merge with Ecto.Query](https://dev.to/ktravers/til-how-to-select-merge-with-ecto-query-1944)

## update
Updates are used to update the filtered entries. In order for updates to be applied, Ecto.Repo.update_all/3 must be invoked.

``` elixir
query = from(u in User, update: [set: [name: "new name"]])
Repo.update_all(query, [], [])
```
## delete

``` elixir
query = from(post in Post, where: post.name == "abc")
Repo.delete_all(query)
```

## catch the ecto error

``` elixir
chset =
  %SomeModel{}
  |> SomeModel.changeset(attrs)
try do
  chset
  |> Repo.insert()
catch :error,  %Postgrex.Error{postgres: %{code: :invalid_password}} ->
  { :error ,
    chset
    |> Changeset.add_error(:username, "may be invalid")
    |> Changeset.add_error(:password, "may be invalid")
  }
else
  {:ok, lr} -> {:ok, Map.put(lr, :password, nil)}
  error -> error
end
```

copy from [Elixir - try/catch vs try/rescue?](https://stackoverflow.com/questions/40280887/elixir-try-catch-vs-try-rescue)
