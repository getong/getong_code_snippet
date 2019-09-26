# Elixir DateTime

## timezone

``` elixir
d1 = DateTime.utc_now()
{:ok, d2} = DateTime.now("Asia/Shanghai")
d2.hour - d1.hour
8

## compare it by map struct
d1 > d2
false

## use the DateTime compare method
DateTime.compare(d1, d2)
:lt
```
