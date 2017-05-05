# elixir map put_in
```
nested = %{
	buttercup: %{
	  actor: %{
		first: "Robin",
		last:  "Wright"
	  },
	  role: "princess"
	},
	westley: %{
	  actor: %{
		first: "Carey",
		last:  "Ewes"     # typo!
	  },
	  role: "farm boy"
	}
}

IO.inspect get_in(nested, [:buttercup])
# => %{actor: %{first: "Robin", last: "Wright"}, role: "princess"}

IO.inspect get_in(nested, [:buttercup, :actor])
# => %{first: "Robin", last: "Wright"}

IO.inspect get_in(nested, [:buttercup, :actor, :fqirst])
# => "Robin"

IO.inspect put_in(nested, [:westley, :actor, :last], "Elwes")
# => %{buttercup: %{actor: %{first: "Robin", last: "Wright"}, role: "princess"},
# =>     westley: %{actor: %{first: "Carey", last: "Elwes"}, role: "farm boy"}}
```

## elixir map get_in
```
authors = [
  %{ name: "José",  language: "Elixir" },
  %{ name: "Matz",  language: "Ruby" },
  %{ name: "Larry", language: "Perl" }
]

languages_with_an_r = fn (:get, collection, next_fn) ->
   for row <- collection do
	 if String.contains?(row.language, "r") do
	   next_fn.(row)
	 end
   end
end

IO.inspect get_in(authors, [languages_with_an_r, :name])
#=> [ "José", nil, "Larry" ]

```
# elixir map update_in
```
defmodule Customer do
  defstruct name: "", company: ""
end

defmodule BugReport do
  defstruct owner: %{}, details: "", severity: 1
end

report = %BugReport{owner: %Customer{name: "Dave", company: "Pragmatic"},
					details: "broken"}

IO.puts inspect report

report = %BugReport{ report | owner: %Customer{ report.owner | company: "PragProg" }}

IO.puts inspect report

IO.puts inspect update_in(report.owner.name, &("Mr. " <> &1))


```
