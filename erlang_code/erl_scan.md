# erl_scan get string type

## tsung use erl_scan:string/1 to get the string's value, like below:

```
getTypeAttr(string, String)-> String;
getTypeAttr(list, String)-> String;
getTypeAttr(float_or_integer, String)->
    case erl_scan:string(String) of
        {ok, [{integer,1,I}],1} -> I;
        {ok, [{float,1,F}],1} -> F
    end;
getTypeAttr(integer_or_string, String)->
    case erl_scan:string(String) of
        {ok, [{integer,1,I}],1} -> I;
        _ -> String
    end;
getTypeAttr(Type, String) ->
    {ok, [{Type,1,Val}],1} = erl_scan:string(String),
    Val.

{ok, [{atom,1,Name}],1} = erl_scan:string("tr_"++RawName),
```
copy from ts_config.erl
The `String` is read from the xml files, and in erlang, it is the list type.
code exec info:

```
$erl
1> erl_scan:string("800").
{ok,[{integer,1,800}],1}

2>erl_scan:string("tr_"++ "abc").
{ok,[{atom,1,tr_abc}],1}
```

## parse string to erlang term

``` erlang
string_to_term(String) when is_list(String) ->
    case erl_scan:string(String++".") of
        {ok, Tokens, _} ->
            case erl_parse:parse_term(Tokens) of
                {ok, Term} ->
					Term;
                _Err ->
					error
			end;
        _Error ->
            error
    end.
```
