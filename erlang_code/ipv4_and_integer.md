# ipv4 and integer
copy from minuteman_lashup_vip_listener.erl
```
-spec(integer_to_ip(IntIP :: 0..4294967295) -> inet:ip4_address()).
integer_to_ip(IntIP) ->
    <<A, B, C, D>> = <<IntIP:32/integer>>,
    {A, B, C, D}.

-spec(ip_to_integer(inet:ip4_address()) -> 0..4294967295).
ip_to_integer(_IP = {A, B, C, D}) ->
    <<IntIP:32/integer>> = <<A, B, C, D>>,
    IntIP.
```
example:

```
1> <<Num:32/integer>> = << 1, 2, 3, 4>>.
<<1,2,3,4>>
2> Num.
16909060
3> round(math:pow(256, 3) + 2 * math:pow(256,  2) + 3 * 256 + 4).
16909060
4> round(math:pow(2, 8)).
256
```
