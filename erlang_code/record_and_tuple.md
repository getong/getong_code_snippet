# record and tuple

## record is represented as tuple in erlang

## erlang running as bad matched error
erlang log reports:
```
Exception:{badmatch,[{cfg_record_name,0,100,[]}]},
```
and the source code is just like this, in the source file named `b.erl`:

```
[#cfg_record_name{a = AValue, b = BValue, c = CValue}] = cfg_record_name:get(Type, Value),
```

and the cfg_record_name module:

```
get(1, 1) ->
    [#cfg_record_name{a = 1, b = 2, c = 3}].
```

and the cfg_record_name redord definition:

``` erlang
-record(cfg_record_name, {
    a,
    b,
    c,
    d
    }).
```
please note that, there is four elements in the `cfg_record_name`, `a`, `b`, `c`, `d`.

Finally, I checkout the reason is that:
in the cfg_record_name.erl belonged directory, there is also a cfg_record_name record in another header file:
``` erlang
-record(cfg_record_name, {
    a,
    b,
    c
    }).
```
and because the header file is just in the same directory, the cfg_record_name.erl is compiled with the header file.

and the `b.erl` is compiled with another header file.

Both source files can be compiled, but they can not be match.
