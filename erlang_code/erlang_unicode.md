# erlang unicode

## erlang reference
Reference to [Erlang Unicode编码](http://wudaijun.com/2016/12/unicode-in-erlang/)


> ASCII: 不用多说，编码空间为7位(0-127)
> ISO 8859-1: 又称Latin-1，以ASCII为基础，在空置的0xA0-0xFF的范围内，加入96个字母及符号。编码空间为8位(0-255)
> UCS-2: 16位编码空间 又称基本多文种平面或零平面
> UCS-4: 32位编码空间 在UCS-2基本上，加入辅助平面(目前有16个辅助平面，至少需要21位)
> 注1: UCS(Universal Character Set, 通用字符集)
> 注2: 以上四种编码都是向前兼容的，通常我们所说的Unicode编码指UCS-2和UCS-4，目前广泛运用的是UCS-2

code:

``` erlang
1> L = "中文".
...
4> UTF8 = unicode:characters_to_binary(L).% 将L中的unicode编码转换为UTF8 binary
<228,184,173,230,150,135>>
5> UTF16Big = unicode:characters_to_binary(UTF8,utf8,utf16).
<<78,45,101,135>> % 默认为Big Endian
6> UTF16Little = unicode:characters_to_binary(UTF8,utf8,{utf16,little}).
<<45,78,135,101>>

% 方案三. 利用binary构造语法构建
7> UTF8 = <<"中文"/utf8>>.
<<228,184,173,230,150,135>>

...
13> io:format("~ts", [UTF8]).
中文ok

```


> iolist: 0-255编码(Latin-1)的lists，binary，或它们的嵌套，如[["123",<<"456">>],<<"789">>]
> unicode binary: UTF8编码的binary(Erlang默认使用UTF8 binary编码unicode)
> charlist: UTF8编码的binary，或包含有效unicode codepoint的lists，或它们的嵌套，如[<<"hello">>, "中国"]

> ~s只能打印iolist，binary，或atom，因此不能直接打印中文lists(无法解码超过255的codepoint)或UTF8 binary(会按字节解释，出现乱码)。

> ~ts则可打印charlist和unicode binary。

> ~p如果不能打印出ASCII(0-127)字符，则直接打印出原生Term，不会对Unicode编码进行处理。

## convert http post data to unicode

``` erlang
utf8_list_to_string(StrangeList) ->
  unicode:characters_to_list(list_to_binary(StrangeList)).
```
[Using Unicode in Erlang](http://erlang.org/doc/apps/stdlib/unicode_usage.html)
[how to support chinese in http request body? erlang](https://stackoverflow.com/questions/21304233/how-to-support-chinese-in-http-request-body-erlang)

## erlang file encode
```erlang
%% coding: latin-1
```
The default is UTF-8, but this can make it use local encode.

## jsx encode utf8 words

``` erlang
UTF8Words = [{<<"word">>, unicode:characters_to_binary("中文")}]，
jsx:encode(UTF8Words)
```
jsx only support list, if the element of the list is tuple, change the tuple element to list.

## unicode regxp pattern match

``` erlang
1> re:run("hello 中国 ren", "[\x{4e00}-\x{9fff}]+", [unicode]).
2> {ok, RegUni} = re:compile("\\p{L}{5}", [unicode]).
 {ok,{re_pattern,0,1,0,
                 <<69,82,67,80,77,0,0,0,0,8,0,0,1,0,0,0,255,255,255,255,
                   255,255,...>>}}
3> re:run(<<"こんにちは"/utf8>>, RegUni).
```
copy from
[Erlang 0062] Erlang Unicode 两三事](https://www.cnblogs.com/me-sa/archive/2012/05/31/erlang-unicode.html)
[erlang-questions Regexp Matching on Unicode](http://erlang.org/pipermail/erlang-questions/2016-December/091115.html)

## jsx decode result is map type in v3.0
But the prior version is a list.
