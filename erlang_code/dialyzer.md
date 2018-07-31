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

## typer
After dialyzer adds apps, it can use typer to generate code spec.

``` shell
typer -r src --show -I _build/default/lib/ -I include
```

## rebar3 dialyzer
see the [rebar3 dialyzer](https://www.rebar3.org/docs/commands#section-dialyzer)

## dialyzer ignore warning file
we can edit the file `dialyzer.ignore-warnings`, the dialyzer will ignore the warning.

``` erlang
lager_trunc_io.erl:283: Call to missing or unexported function erlang:is_map/1
lager_trunc_io.erl:335: Call to missing or unexported function erlang:map_size/1
Unknown functions:
  lager_default_tracer:info/1
    maps:to_list/1
```
copy from [lager](https://github.com/erlang-lager/lager)
