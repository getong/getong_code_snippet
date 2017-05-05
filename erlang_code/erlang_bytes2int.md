# erlang bytes2int
copy from mnesia_locaker.erl

```
bytes2int(N1, N0) when 0 =< N1, N1 =< 255,
		       0 =< N0, N0 =< 255 ->
    (N1 bsl 8) bor N0.
bytes2int(N3, N2, N1, N0) when 0 =< N3, N3 =< 255,
			       0 =< N2, N2 =< 255,
			       0 =< N1, N1 =< 255,
			       0 =< N0, N0 =< 255 ->
    (N3 bsl 24) bor (N2 bsl 16) bor (N1 bsl 8) bor N0.
```

code example

```
$erl
1> Bin = <<4, 5, 6, 7>>.
2> 4*math:pow(256, 3) + 5 * math:pow(256, 2) + 6*256 +7.
67438087.0
3> (4 bsl 24) bor (5 bsl 16) bor (6 bsl 8) bor 7.
67438087
4> Bin2 = <<6, 7>>.
5> (6 bsl 8) bor 7.
1543
6> 6 * math:pow(256, 1) + 7.
1543
7> 1 bsl 8.
256
8> 256 bsr 1.
128
9>

```
bsl操作可以当作乘以2的幂。
bsr 操作可以认为除于2的幂。
当bor的左右2个数字差距超过8，也就是它们的二进制表示没有重疊的1表示时，可以认为是相加。
