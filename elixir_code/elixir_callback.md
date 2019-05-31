# elixir callback

``` elixir
@optional_callbacks handle_in: 3,
                      handle_out: 3,
                      handle_info: 2,
                      handle_call: 3,
                      handle_cast: 2,
                      code_change: 3,
                      terminate: 2

@callback join(topic :: binary, payload :: map, socket :: Socket.t()) ::
              {:ok, Socket.t()}
              | {:ok, reply :: map, Socket.t()}
              | {:error, reason :: map}
```
copy from phoenix/lib/phoenix/channel.ex
