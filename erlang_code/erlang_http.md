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
