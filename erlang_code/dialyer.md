# dialyzer

## build

``` shell
dialyzer --build_plt --apps erts kernel stdlib
```

## append other applications

``` shell
dialyzer --add_to_plt --apps crypto compiler public_key
```

## check the source code

``` shell
dialyzer -r src --src
dialyzer -r src test --src
dialyzer -r src test -I include --src
```
or use the rebar3:

``` shell
rebar3 dialyzer
```
or check the ebin

``` shell
dialyzer -r ebin
```
