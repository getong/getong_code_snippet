# erlang http

## four http content types
* application/x-www-form-urlencoded
* multipart/form-data
* application/json
* text/xml

`application/x-www-form-urlencoded` will escape some special charactors, use `application/json`.
see [四种常见的 POST 提交数据方式](https://imququ.com/post/four-ways-to-post-data-in-http.html)

## erlang post method

``` shell
http_post_content(Url, Content) ->
	ContentType = "application/json",
	%% Concat the list of (character) lists
	Body = lists:concat(["{\"content\":\"", Content, "\"}"]),
	%% Explicitly encode to UTF8 before sending
	UnicodeBin = unicode:characters_to_binary(Body),
	httpc:request(post,
		{
			Url,
			[],          % HTTP headers
			ContentType, % content-type
			UnicodeBin   % the body as binary (UTF8)
			},
		[],            % HTTP Options
		[{body_format,binary}] % indicate the body is already binary
		).
```

see [how-to-support-chinese-in-http-request-body-erlang](https://gist.github.com/flitbit/11388377)

## mochiweb
`application/x-www-form-urlencoded` use Req:parse_post()
`application/json` use Req:recv_body
see [MOCHIWEB参数化模型REQ相关功能](http://coolshell.cn/articles/1516.html)
see [Using mochiweb to create a web framework in erlang](http://willcodeforfoo.com/2009/07/using-mochiweb-to-create-a-web-framework-in-erlang)
see [Simple JSON request with cURL to Mochiweb](https://stackoverflow.com/questions/10439603/simple-json-request-with-curl-to-mochiweb)

## hackney & gun
hackney is an HTTP client library, it offers a pool client feartur.
gun is also  HTTP client library, and it supports websocket.

``` erlang
application:start(crypto),
application:start(public_key),
application:start(ssl),
application:start(hackney).
PoolName = mypool,
Options = [{timeout, 150000}, {max_connections, 100}],
ok = hackney_pool:start_pool(PoolName, Options),


Method = get,
URL = <<"https://friendpaste.com">>,
Headers = [],
Payload = <<>>,
Options = [{pool, default}],
{ok, StatusCode, RespHeaders, ClientRef} = hackney:request(Method, URL, Headers,
                                                        Payload, Options).
```
copy from [hackney](https://github.com/benoitc/hackney)

## username and password
curl example:
``` shell
curl --user name:password http://www.example.com
curl -u name:password http://www.example.com
```
copy from [curl设置http头Authentication实现http基本认证](https://blog.csdn.net/bytxl/article/details/50379488)
hackney example:

``` shell
Url = <<"http://localhost:8000/basic-auth/username/password">>,
Options = [{basic_auth, {<<"username">>, <<"password">>}}],
{ok, StatusCode, _, _} = hackney:request(get, URL, [], <<>>, Options)
```
copy from Basic auth with Hackney](https://lookonmyworks.co.uk/2015/02/19/basic-auth-with-hackney/)

## elastic example

``` shell
## curl
curl --user user:password -H "Content-Type: application/json" -X PUT  http://127.0.0.1:9200/_bulk -d '
{ "index" : { "_index" : "table_name", "_type" : "_doc"} }
{"a":"1","b":"2"}
'
## erlang
hackney:request(put, <<"http://127.0.0.1:9200/_bulk">>, [{<<"Content-Type">>, <<"application/json">>}, {<<"connection">>, <<"keep-alive">>}], jsx:encode({"a" => 1, "b" => 2}), []).
```

## query string

``` shell
%% @doc calculate canonical query string out of query params and according to v4 documentation
canonical_query_string([]) ->
    "";
canonical_query_string(Params) ->
    Normalized = [{nodefinder_ec2_api_http:url_encode(Name), nodefinder_ec2_api_http:url_encode(nodefinder_ec2_api_http:value_to_string(Value))} || {Name, Value} <- Params],
    Sorted = lists:keysort(1, Normalized),
    lists:join($&,
               [case Value of
                    [] -> [Key, "="];
                    _ -> [Key, "=", Value]
                end
                || {Key, Value} <- Sorted, Value =/= none, Value =/= undefined]).
```
copy from nodefinder_ec2_api_aws.erl
