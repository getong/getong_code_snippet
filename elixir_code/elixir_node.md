#elixir node
## a node
```
iex --sname a
1> Node.self()
:b@DebianLighten

```
## b node
```
iex --sname b
iex(b@DebianLighten)1> Node.self()
:b@DebianLighten
iex(b@DebianLighten)2>

```
## connection
```
iex(b@DebianLighten)2> Node.connect :a@DebianLighten
true
```
in a node
```
iex(a@DebianLighten)2> Node.list
[:b@DebianLighten]
```
in b node
```
iex(b@DebianLighten)3> Node.list
[:a@DebianLighten]
```
