#elixir control flow
##if
```
if 1 == 1, do: "true part", else: "false part"

if 1 == 1 do
	"true part"
else
	"false part"
end
```
##unless
```
unless 1 == 1, do: "error", else: "OK"

unless 1 == 2 do
	"OK"
else
	"error"
end
```
##cond
```
cond do
	rem(current, 3) == 0 and rem(current, 5) == 0 ->
		"FizzBuzz"
    rem(current, 3) == 0 ->
		"Fizz"
	rem(current, 5) == 0 ->
		"Buzz"
	true ->
		current
end
```
##case
```
case File.open("filename") do
	{:ok, file} ->
		ok
	{:error, reason} ->
		error
end
```
