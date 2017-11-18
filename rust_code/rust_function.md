# rust function

## pattern match

``` rust
fn print_id((name, age): (&str, i32)) {
println!("I'm {},age {}.", name, age);
}

fn print_age((_, age): (&str, i32)) {
println!("My age is  {}", age);
}

fn print_name((name,_): (&str, i32)) {
println!("I am  {}", name);
}
```
see [函数参数](https://github.com/rustcc/RustPrimer/blob/master/function/arguement.md)


## function trait

``` rust
fn process<F>(n: i32, func: F) -> i32
	where F: Fn(i32) -> i32 {
	func(n)
}
```

## function lifetime

``` rust
fn foo<'a>(x :&'a str) -> &'a str {
	x
}
```

## devive属性

``` rust
#[devive]
struct Foo;

fn main() {
	println!!("{:?}", Foo);
}

```

## 泛型函数

``` rust
use std::ops::Add;

fn add<T: Add<T, Output=T>>(a:T, b:T) -> T {
	a + b
}

fn main() {
	println!("{}", add(100i32, 1i32));
	println!("{}", add(100.11f32, 100.22f32));
}
```

## Copy

``` rust
实现Copy  的基本数据类型有： i8, i16, 832, 864, usize, u8, u32, u64, f32, f64, {}, bool, char等
结构体类型的属性类型都实现 Copy特性，那么这个类型就可以实现Copy特性。
```

## move
move 关键字常用在闭包中， 强制闭包获取所有权。
没有实现Copy特性的类型默认都是move 特性
``` rust
let mut num = 5;
let mut plus_num = move |x: i32| num += x
plus_num(5);

let s = String::form(“hello”);
let b = s += “ world”;  // can not use s anymore

let 标识符A = 标识符B;
```
move + copy 可以获取环境变量

``` rust
fn factory() -> Box<Fn(i32) -> i32> {
	let num = 5;
	Box::new(move |x| x + num)
}

let f = factory();
let anser = f(1);
assert_eq!(6, answer);
```


## higher order function

``` rust
fn main() {
   let a = [1,2,3,4,5,6,7];
   let mut b = Vec::<i32>::new();
   for i in &a {
       b.push(get_func(*i)(*i));
   }
   println!("{:?}", b);
}

fn get_func(n: i32) -> fn(i32) -> i32 {
    fn inc(n: i32) -> i32 {
        n + 1
    }
    fn dec(n: i32) -> i32 {
        n - 1
    }
    if n % 2 == 0 {
        inc
    } else {
        dec
    }
}
```
The `Vec::<i32>::new()` return a vector list.
The `for i in &a` and the `i` is need to deref, using `*i`.
The function can be defined inside another function.
The return statement can just return the function name as a result.

## 在rust 中，当一个函数返回 () 时，可以省略。
main函数的返回值类型是(), 它是一个特殊的元组--没有元素的元组，称为unit， 它表示一个函数没有任何信息需要返回。
rust函数不支持多返回值，但可以利用元组来返回多个值。
发散函数（diverging function） 是rust中的一个特性，发散函数不返回，它使用感叹号！作为返回类型表示。

``` rust
fn main() {
    println!("hello");
	diverging();
	//println!("world");
}

fn diverging() -> ! {
      panic!("This function will never return ");
  }
```

## thread
`thread::spawn` 函数需要一个函数作为参数，而且是 `FnOnce` 类型，所以一般会有 `move` 作为开始。


## derive
trait can be derived.

## unwrap
Rust 中“unwrap”是说，“给我计算的结果，并且如果有错误，panic 并停止程序。”
see [错误处理](https://kaisery.gitbooks.io/rust-book-chinese/content/content/Error%20Handling%20%E9%94%99%E8%AF%AF%E5%A4%84%E7%90%86.html)

## Option
Option 是Rust的系统类型，用来表示值不存在的可能。
``` rust
enum Option<T> {
	None,
	Some(T),
}
```

## fmt::Debug, fmt::Display
fmt::Debug 可以打印任何类型数据，但不可控制输出格式。
fmt::Display 可以控制格式，但需要对类型数据进行impl 操作。


## reverve some vector element

``` rust
let mut v: Vec<usize> = vec![];
v.reserve(5_000_000);
```
In rust  1.21,  reverve function not make a different.

## sync_channel is synchronous, channel is asynchronous
std::sync::mpsc::sync_channel is synchronus, and has the buffer size.
std::sync::mpsc::channel is asynchronus.

## std::mem

``` rust
use std::mem;

let mut n = 0;
let mut m = 1;
mem::swap(&mut n, &mut m);
mem::size_of_val(&mut n)
mem::size_of::<isize>();
```

## box
>> Box, 以及栈和堆
>>
>> 在 Rust 中，所有值默认都由栈分配。值也可以通过创建 Box<T> 来装箱（boxed，分配在堆上）。装箱类型是一个智能指针，指向堆分配的 T 类型的值。当一个装箱类型离开作用域时，它的析构器会被调用，内部的对象会被销毁，分配在堆上内存会被释放。

>> 装箱的值可以使用 * 运算符进行解引用；这会移除掉一个间接层（this removes one layer of indirection. ）。
