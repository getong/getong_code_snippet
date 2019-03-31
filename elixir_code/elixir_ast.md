# elixir ast
All the elixir code is represented as this:

```
{ tuple | atom, list, list | atom }
```

## macro

```
defmacro macro(code) do
  IO.inspect Code
  code
end

macro(IO.puts(“hello”))

```