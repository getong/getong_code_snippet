# elixir func
通常，在单行代码使用do: 在多行代码使用do...end 语法
## do...end

```
%% times.exs
defmodule Times do
	def double(n) do
		n * 2
	end
end
```
## do:

```
defmodule Times do
	def double(n), do: n * 2
end
```
