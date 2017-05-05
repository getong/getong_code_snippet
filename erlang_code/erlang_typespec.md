# erlang typespec
erlang typespec is defined is doc/reference_manual/typespec.html, online website is
[Types and Function Spectifications](http://erlang.org/doc/reference_manual/typespec.html)
for example, the iolist is defined:

```
iolist()::maybe_improper_list(byte() | binary() | iolist(), binary() | [])
```
use iolist for performance.
