# struct
```
defmodule Db do

  defstruct [
    name: nil,
    style: nil,
    color: nil,
    opts: %{}
  ]

end

defmodule Db2 do

  defstruct name: nil,
    style: nil, color: nil, opts: %{}

end
```
struct is very like the map type, but the struct often works with the module name.

## struct to map

``` elixir
Map.from_struct(struct)
```
