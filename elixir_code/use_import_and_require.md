#elixir use, import and require
[参考 Elixir:use,import和require](https://segmentfault.com/a/1190000004514268)
# 概述
>`use Module` 除了在`Module`上调用 `__using__` 宏外, 不做其他任何事情.
>`import Module`  会把`Module`模块的所有非私有函数和宏引入到当前模块中, 可以直接通过名字调用函数和宏.
>`require Module` 允许使用一个模块的宏, 但并不导入他们. 必须通过全名(带名称空间)来引用.

# 模块中的宏， 可以通过import 或者 require导入
