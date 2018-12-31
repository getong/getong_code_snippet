# elixir process

## proces info
``` elixir
pid = spawn(fn -> Process.sleep(:infinity))

Porcess.info(pid)[:message_queue_len]
Process.info(pid)
```
