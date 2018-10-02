# zlib

## general code

```
Z = zlib:open(),
zlib:deflateInit(Z, default, deflated, 31, 8, default),
Data = zlib:deflate(Z, Data0),
Data = zlib:deflate(Z, Data0, finish),
zlib:deflateEnd(Z),
zlib:close(Z),
```
