#erlang unique_integer

``` erlang
$erl
1> erlang:unique_integer().
-576460752303423484
2> erlang:unique_integer().
-576460752303423476
3>
3>
3> erlang:unique_integer([positive, monotonic]).
1
4> erlang:unique_integer([positive, monotonic]).
2
5> erlang:unique_integer([positive, monotonic]).
3
6> erlang:unique_integer([positive, monotonic]).
4
7> erlang:unique_integer([monotonic]).
-576460752303423484
8> erlang:unique_integer([monotonic]).
-576460752303423483
9> erlang:unique_integer([monotonic]).
-576460752303423482
10> erlang:unique_integer([monotonic]).
-576460752303423481
```
erlang:unique_integer/0 just returns a unique integer without order, and erlang:unique_integer/1 uses parameters to return ordered integers.
