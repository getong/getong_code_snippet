# elixir Logger

## basic usage

``` elixir
config :logger,
  level: :info,
  handle_sasl_reports: true,
  backends: [:console]

config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]

Logger.debug("I am debugging info")
Logger.info("I am info-level message! Probably ok.")
Logger.warn("I am a warning! Beware!")
Logger.error("Oh no, this is not good :(")
```


## enhance

```
def deps do
  [{:logger_papertrail_backend, "~> 1.1"}]
end

config :logger, :logger_papertrail_backend,
  url: "papertrail://logs.papertrailapp.com:<port>/<system_name>",
  level: :info,
  format: "$metadata $message"


config :logger,
  backends: [ :console,
    LoggerPapertrailBackend.Logger
  ]
```

## metadata
``` elixir
...

defp set_request_id({conn, request_id}, header) do
    Logger.metadata(request_id: request_id)
    Conn.put_resp_header(conn, header, request_id)
end

...


config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id],
  level: :info

```
another example
``` elixir
...

Logger.metadata(user_id: user_id)

...


config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id, user_id],
  level: :info

```

## Important thing to know about Logger
```

async/sync modes
switches to sync when buffer is full
use own log statements but *don't abuse it*
use own metadata but *don't abuse it*
can track metrics (but you probably want to use :telemetry instead)
abusing logger will result in performance punishment to your production environment!


rely on OTP 21+ :logger
backward-compatible with current Logger (for now)
allow structured logging
allow sharing metadata with Erlang logs
new log levels (:emergency, :alert, :critical, :warning, :notice)
handlers written in Erlang (or Elixir!)
built-in logging to files (with file rotation!)
smooth integration with Lager (erlang-lager/lager)
```
copy from [Keeping tabs on production](https://slides.com/hubertlepicki/keeping-tabs-on-production)