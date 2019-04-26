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
