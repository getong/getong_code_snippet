# rust trait

## PartialOrd , PartialEq

```
比较运算符实际上也是某些 trait 的语法糖，不过比较运算符所实现的 trait 只有2个：std::cmp::PartialEq和 std::cmp::PartialOrd。

其中，==和!= 实现的是 PartialEq，<、>、>= 和 <=实现的是 PartialOrd。

标准库中，std::cmp 这个 mod 下有4个 trait，而且直观来看 Ord 和 Eq 岂不是更好？但 Rust 对于这4个 trait 的处理是很明确的。因为在浮点数有一个特殊的值叫 NaN，这个值表示未定义的一个浮点数。在 Rust 中可以用0.0f32 / 0.0f32来求得其值，这个数是一个都确定的值，但它表示的是一个不确定的数，那么NaN != NaN 的结果是啥？标准库告诉我们是 true。但这么写有不符合Eq定义里的total equal（每位一样两个数就一样）的定义。因此有了 PartialEq这么一个定义，NaN 这个情况就给它特指了。

为了普适的情况，Rust 的编译器就选择了PartialOrd 和PartialEq来作为其默认的比较符号的trait。
```

## Iterator

```
iter()	返回一个只读可重入迭代器，迭代器元素的类型为 &T
into_iter()	返回一个只读不可重入迭代器，迭代器元素的类型为 T
iter_mut()	返回一个可修改可重入迭代器，迭代器元素的类型为 &mut T
```
copy from [Rust 迭代器 Iterator](https://www.twle.cn/c/yufei/rust/rust-basic-iterator.html)

## Fn, FnMut, FnOnce

``` rust
#[lang = "fn_once"]
pub trait FnOnce<Args> {
    type Output;
    extern "rust-call" fn call_once(self, args: Args) -> Self::Output;
}

#[lang = "fn_mut"]
pub trait FnMut<Args>: FnOnce<Args> {
    extern "rust-call" fn call_mut(&mut self, args: Args) -> Self::Output;
}

#[lang = "fn"]
pub trait Fn<Args>: FnMut<Args> {
    extern "rust-call" fn call(&self, args: Args) -> Self::Output;
}
```

```
FnOnce, 参数类型是 self，所以，这种类型的闭包会获取变量的所有权，生命周期只能是当前作用域，之后就会被释放了。
FnMut, 参数类型是 &mut self，所以，这种类型的闭包是可变借用，会改变变量，但不会释放该变量。所以可以运行多次。
Fn, 参数类型是 &self，所以，这种类型的闭包是不可变借用，不会改变变量，也不会释放该变量。所以可以运行多次。
```
copy from [谈一谈Fn, FnMut, FnOnce的区别](https://www.dazhuanlan.com/2019/12/09/5dee50f786c92/)

## From, Into trait

From trait
``` rust
use std::convert::From;

#[derive(Debug)]
struct Number {
    value: i32,
}

impl From<i32> for Number {
    fn from(item: i32) -> Self {
        Number { value: item }
    }
}

fn main() {
    let num = Number::from(30);
    println!("My number is {:?}", num);
}

```
Into trait

``` rust
use std::convert::From;

#[derive(Debug)]
struct Number {
    value: i32,
}

impl From<i32> for Number {
    fn from(item: i32) -> Self {
        Number { value: item }
    }
}

fn main() {
    let int = 5;
    let num: Number = int.into();
    println!("My number is {:?}", num);
}
```
如果你为你的类型实现了 From，那么同时你也就免费获得了 Into。

使用 Into trait 通常要求指明要转换到的类型，因为编译器大多数时候不能推断它。
copy from [From 和 Into](https://rustwiki.org/zh-CN/rust-by-example/conversion/from_into.html)


## Drop trait

``` rust
struct HasDrop;

impl Drop for HasDrop {
    fn drop(&mut self) {
        println!("Dropping HasDrop!");
    }
}

struct HasTwoDrops {
    one: HasDrop,
    two: HasDrop,
}

impl Drop for HasTwoDrops {
    fn drop(&mut self) {
        println!("Dropping HasTwoDrops!");
    }
}

fn main() {
    let _x = HasTwoDrops { one: HasDrop, two: HasDrop };
    println!("Running!");
}
```
output

```
Running!
Dropping HasTwoDrops!
Dropping HasDrop!
Dropping HasDrop!
```

function:

``` rust
fn drop(&mut self)
```

```
Copy and Drop are exclusive
You cannot implement both Copy and Drop on the same type. Types that are Copy get implicitly duplicated by the compiler, making it very hard to predict when, and how often destructors will be executed. As such, these types cannot have destructors.
```
copy from [Trait std::ops::Drop](https://doc.rust-lang.org/std/ops/trait.Drop.html)
