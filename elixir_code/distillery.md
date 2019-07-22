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
mix distillery.init
mix distillery.release
mix distillery.release.clean
mix distillery.gen.appup
```
