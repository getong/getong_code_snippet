#elixir docs
[hexdocs](https://hexdocs.pm/)
[30-days-of-elixir](https://github.com/seven1m/30-days-of-elixir)

# elixir is a erlang application, and can be started in other erlang applications

``` erlang
start_elixir_application() ->
    case ejabberd_config:is_elixir_enabled() of
	true ->
	    case application:ensure_started(elixir) of
		ok -> ok;
		{error, _Msg} -> ?ERROR_MSG("Elixir application not started.", [])
	    end;
	_ ->
	    ok
    end.
```
