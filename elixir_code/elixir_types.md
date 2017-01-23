#elixir types
## keyword list 列表
```
$iex
iex(1)> [ name: "Dave", city: "Dallas", likes: "Programming" ]
[name: "Dave", city: "Dallas", likes: "Programming"]
iex(2)> [1, fred: 1, dave: 2]
[1, {:fred, 1}, {:dave, 2}]
iex(3)> {1, fred: 1, dave: 2}
{1, [fred: 1, dave: 2]}

```
## map 散列表

```
$iex
iex(1)> %{ "AL" => "Alabama", "WI" => "Wisconsin" }
%{"AL" => "Alabama", "WI" => "Wisconsin"}
iex(2)> responses = %{ { :error, :enoent } => :fatal, { :error, :busy } => :retry }
%{{:error, :busy} => :retry, {:error, :enoent} => :fatal}
iex(3)> colors = %{ :red => 0xff000, :green => 0x00ff00, :blue => 0x0000ff }
%{blue: 255, green: 65280, red: 1044480}
iex(4)> %{"one" => 1, :two => 2, {1,1,1} => 3}
%{:two => 2, {1, 1, 1} => 3, "one" => 1}
iex(5)> colors = %{ red: 0xff0000, green: 0x00ff00, blue: 0x0000ff}
%{blue: 255, green: 65280, red: 16711680}
```
## get map values 访问散列表

```
$iex
iex(1)> states = %{"AL" => "Alabama", "WI" => "Wisconsin" }
%{"AL" => "Alabama", "WI" => "Wisconsin"}
iex(2)> states["AL"]
"Alabama"
iex(3)> state["TX"]
warning: variable "state" does not exist and is being expanded to "state()", please use parentheses to remove the ambiguity or change the variable name
iex:3
** (CompileError) iex:3: undefined function state/0
(stdlib) lists.erl:1354: :lists.mapfoldl/3
iex(3)> states["TX"]
nil
iex(4)> response_types = %{ { :error, :enoent } => :fatal,
	...(4)> {:error, :busy} => :retry}
%{{:error, :busy} => :retry, {:error, :enoent} => :fatal}
iex(5)> response_types[{:error, :busy}]
:retry
iex(6)>
nil
iex(7)>
nil
iex(8)>
nil
iex(9)> colors = %{ red: 0xff0000, green: 0x00ff00, blue: 0x0000ff }
%{blue: 255, green: 65280, red: 16711680}
iex(10)> clors[:red]
warning: variable "clors" does not exist and is being expanded to "clors()", please use parentheses to remove the ambiguity or change the variable name
iex:10

** (CompileError) iex:10: undefined function clors/0
(stdlib) lists.erl:1354: :lists.mapfoldl/3
iex(10)> colors[:red]
16711680
iex(11)> colors.green
65280

	```
