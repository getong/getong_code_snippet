# erlang typespec
erlang typespec is defined is doc/reference_manual/typespec.html, online website is
[Types and Function Spectifications](http://erlang.org/doc/reference_manual/typespec.html)
for example, the iolist is defined:

```
iolist()::maybe_improper_list(byte() | binary() | iolist(), binary() | [])
```
An iolist is a list containing binaries, characters, strings or other iolists.
use iolist for performance.

## binary and bitsting
The difference between in a binary and a bitstring is that the length of a binary is evenly divisible by 8, i.e. it contains no 'partial' bytes; a bitstring has no such restriction.

copy from [What is the difference between a Binary and a Bitstring in Erlang?](https://stackoverflow.com/questions/10820971/what-is-the-difference-between-a-binary-and-a-bitstring-in-erlang)
