# deno

## installation

``` shell
## linux
curl -fsSL https://deno.land/x/install/install.sh | sh
## macos
brew install deno

```

## example

``` shell
deno run https://deno.land/std/examples/welcome.ts
```


## migration to deno

``` shell

## app.sh
#!/bin/bash
deno run -A myCode.ts

pm2 start app.sh

## fmt
deno fmt

## test
deno test
```
copy from [手把手教你从 Node 快速迁移到 Deno](https://www.infoq.cn/article/IAHih5jdk8vslmaK10s4)
[From Node to Deno](https://aralroca.com/blog/from-node-to-deno)
