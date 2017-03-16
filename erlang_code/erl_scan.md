#erl_scan get string type

tsung use erl_scan:string/1 to get the string's value, like below:

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
```
copy from ts_config.erl
The `String` is read from the xml files, and in erlang, it is the list type.
code exec info:

```
$erl
1> erl_scan:string("800").
{ok,[{integer,1,800}],1}
```
