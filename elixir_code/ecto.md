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
