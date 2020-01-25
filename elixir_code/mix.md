# mix

## mix commands
```
mix new project_name
mix new —sup project_name
mix deps.get
mix compile
mix format
mix format filename
mix release
mix local.hex
mix local.rebar
mix clean
mix deps.update --all
mix run -e "Lib.fun()"
## umbrella project, then cd apps and create other elixie project
mix new --umbrella project
# mix release

## search the packages about react
mix hex.search react

mix hex.repo list
mix hex.repo set hexpm --url https://hexpm.upyun.com
```

## add erl_otps

```
def project do
  [...,
  erlc_options:  [:no_debug_info, {:i, 'myinclude'}],
  ...]
end
```
copy from [Allow mix to pass compiler options to erlang](https://github.com/elixir-lang/elixir/issues/2665)
and compile like below:

``` shell
mix deps.get
MIX_ENV=prod mix compile --no-debug-info
MIX_ENV=prod mix release
```

## elixir 1.9, the Config module has moved to elixir.
the before
``` elixir
use Mix.Config
```
and now

``` elixir
import Config
```
see [Config](https://hexdocs.pm/elixir/Config.html)

## elixirc_options

``` elixir
# in the mix.exs
def project do
[
...
erlc_options: [{:parse_transform, :lager_transform}, :warn_missing_spec, :warnings_as_errors],
elixirc_options: [warnings_as_errors: true]
...
]
end
```
