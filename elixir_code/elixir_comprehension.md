#elixir comprehension
```
result = for generator or filter... [, into: value ], do: expression
```
example
```
iex(1)> for x <- [ 1, 2, 3, 4, 5], do: x * x
[1, 4, 9, 16, 25]
iex(2)> for x <- [1,2,3,4,5], x < 4, do: x * x
[1, 4, 9]
iex(3)> for << ch <- "hello" >>, do: ch
'hello'
iex(4)> for << ch <- "hello" >>, do: <<ch>>
["h", "e", "l", "l", "o"]
iex(5)> for << << b1::size(2), b2::size(3), b3::size(3) >> <- "hello">>, do: "0#{b1}#{b2}#{b3}"
["0150", "0145", "0154", "0154", "0157"]
iex(6)> for x <- ~w{ cat dog }, into: %{}, do: { x, String.upcase(x)}
%{"cat" => "CAT", "dog" => "DOG"}
iex(7)> for x <- ~w{ cat dog }, into: %{"ant" => "ANT"}, do: { x, String.upcase(x)}
%{"ant" => "ANT", "cat" => "CAT", "dog" => "DOG"}
```
