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
mix run -e "Lib.fun()"
## umbrella project, then cd apps and create other elixie project
mix new --umbrella project
## might be in elixir 1.9 without distillery
mix release

## search the packages about react
mix hex.search react
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
