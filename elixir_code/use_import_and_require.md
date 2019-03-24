# elixir use, import and require, alias
[参考 Elixir:use,import和require](https://segmentfault.com/a/1190000004514268)
## 概述
>`use Module` 除了在`Module`上调用 `__using__` 宏外, 不做其他任何事情.
>`import Module`  会把`Module`模块的所有非私有函数和宏引入到当前模块中, 可以直接通过名字调用函数和宏.
>`require Module` 允许使用一个模块的宏, 但并不导入他们. 必须通过全名(带名称空间)来引用.

## 模块中的宏， 可以通过import 或者 require导入
code example
import
```
$iex
iex(1)> import Bitwise
iex(2)> 2 >>> 4
0
iex(3)> 3 <<< 4
48
```

## require
```
$iex
iex(1)> require Bitwise
Bitwise
iex(2)> Bitwise.<<< 1, 2
4
iex(3)> Bitwise.<<<(1, 2)
4
iex(4)> Bitwise.>>> 1, 2
0
iex(5)>Bitwise.>>>(1, 2)
0
iex(6)> Bitwise.>>> (1, 2)
** (SyntaxError) iex:7: unexpected parentheses. If you are making a function call, do not insert spaces between the function name and the opening parentheses. Syntax error before: '(')
```

## alias
alias will shorten the module name or a new different name.
