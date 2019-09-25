# Elixir DateTime

## timezone

``` elixir
d1 = DateTime.utc_now()
d2 = DateTime.now("Asia/Shanghai")
d2.hour - d1.hour
```
