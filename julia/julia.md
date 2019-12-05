# julia

## zero() and one

``` julia
zero(Int8)
one(Int8)
```
## map

``` julia
map(round, [1.2, 3.5, 1.7])
```
## keywords

``` julia
类型声明： abstract, primitive, type, struct, function, macro, new
权限标识： global, local, mutable, const, outer
模块操作： module, baremodule, using, import, export
逻辑结构： where, for, while, break, continue, if, elseif, else, in
语句块：   begin, quote, let, end, do
混合编程： ccall
```

## random function

``` julia
## 均匀分布的随机数
rand()
## 整态分布的随机数
randn()

## 100个Float64元素列表
rand(100)
## 100个Float32元素列表
rand(Float32, 100)

## 100个float64元素列表
randn(100)
## 100个Float32元素列表
randn(Float32, 100)

## randexp
using Random
randexp()
```

## do syntax

``` julia
map([1,2,3]) do x
2x
end
```
