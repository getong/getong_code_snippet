# elixir Enum Stream
Enum模块每次运算都会产生新的数值，而Stream不会。使用Stream计算，然后使用Enum调用获取最后的结果。
a example see [Processing Large CSV files with Elixir
Streams](https://www.poeticoding.com/processing-large-csv-files-with-elixir-streams/)

## range is a stream

## Stream.run

``` elixir
File.stream!("/path/to/file")
|> Stream.map(&String.replace(&1, "#", "%"))
|> Stream.into(File.stream!("/path/to/other/file"))
|> Stream.run()
```

## Enum.at
The `Enum.at` is the `lists:nth/2` in erlang.

## group_by

``` elixir
Enum.group_by(list, fn {key, _value} -> key end, fn {_key, value} -> value end)
```
