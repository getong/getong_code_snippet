# distrillery

## installation

``` elixir
defp deps do
  [
    {:distillery, "~> 2.0"}
  ]
end
```

## release

``` elixir
mix release.init
mix release
# will tar the target system
MIX_ENV=prod mix release
```
