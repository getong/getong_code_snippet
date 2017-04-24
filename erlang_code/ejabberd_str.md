# ejabberd str module

In ejabberd, the `str.erl` module offers a lot functions that acts like strings in other language.
The `str.erl` use `binary` to represent strings, which `elixir` does it as well.
Some code examples:

``` erlang
$erl
1> binary:copy(<<"abc">>).
<<"abc">>
2> binary:copy(<<"abc">>, 2).
<<"abcabc">>
3>iolist_to_binary(string:left(binary_to_list(<<"abc">>), 2, 3)).
<<"ab">>
```
