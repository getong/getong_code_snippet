# bit syntax
[Bit Syntax](http://erlang.org/doc/programming_examples/bit_syntax.html)

```

Value:Size/TypeSpecifierList

The Size or the TypeSpecifier, or both, can be omitted. Thus, the following variants are allowed:

- Value
- Value:Size
- Value/TypeSpecifierList

The TypeSpecifierList is a list of type specifiers separated by hyphens.

Type
The type can be integer, float, or binary.

Signedness
The signedness specification can be either signed or unsigned. Notice that signedness only matters for matching.

Endianness
The endianness specification can be either big, little, or native. Native-endian means that the endian is resolved at load time, to be either big-endian or little-endian, depending on what is "native" for the CPU that the Erlang machine is run on.

Unit
The unit size is given as unit:IntegerLiteral. The allowed range is 1-256. It is multiplied by the Size specifier to give the effective size of the segment. Since Erlang R12B, the unit size specifies the alignment for binary segments without size.

Example:

X:4/little-signed-integer-unit:8

This element has a total size of 4*8 = 32 bits, and it contains a signed integer in little-endian order.

$erl
1><<3:4/little-signed-integer-unit:8>>.
<<3,0,0,0>>
2> <<10:176/integer>>.
<<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,10>>
3><<10:2/integer-unit:8>>.
<<0,10>>
4><< _:7/binary, Keep:10/binary, _/bits >> = <<10:176/integer>>.
5> Keep.
<<0,0,0,0,0,0,0,0,0,0>>
```


## concatenate two binaries in erlang

``` erlang
28> B1= <<1,2>>.
<<1,2>>
29> B2= <<3,4>>.
<<3,4>>
30> B3= <<B1/binary, B2/binary>>.
<<1,2,3,4>>

31> B4 = << "abc", "cdef", B2/binary>>.
```
see [How do I concatenate two binaries in Erlang?](https://stackoverflow.com/questions/600642/how-do-i-concatenate-two-binaries-in-erlang)


## mongodb object_id

``` erlang
%% @private
-spec oid_machineprocid() -> <<_:40>>.
oid_machineprocid() ->
    OSPid = list_to_integer(os:getpid()),
    {ok, Hostname} = inet:gethostname(),
    <<MachineId:3/binary, _/binary>> = erlang:md5(Hostname),
    <<MachineId:3/binary, OSPid:16/big>>.

%% @doc Fresh object id
-spec object_id() -> bson:objectid().
object_id() ->
    Now = bson:unixtime_to_secs(bson:timenow()),
    MPid = ets:lookup_element(?MODULE, oid_machineprocid, 2),
    N = ets:update_counter(?MODULE, oid_counter, 1),
    bson:objectid(Now, MPid, N).
```
