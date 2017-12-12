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

``` rust
impl<T> Option<T> {
    fn unwrap(self) -> T {
        match self {
            Option::Some(val) => val,
            Option::None =>
              panic!("called `Option::unwrap()` on a `None` value"),
        }
    }
}
```
很多情况下，Rust返回结果可能会出现2种情况，1种正常情况有数值，另外一种情况为没有数值。为了应付这2种情况，Rust的返回值类型为Option，正常情况对应Some，异常情况对应None。 unwrap函数可以获取Some里面包含的数值。

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

## ref
使用ref 关键字来得到一个引用

``` rust
let x = 5;
let mut y = 5;
match x {
	ref r => println!("Got a reference to {}", r),
}

match y {
	ref mut mr => println!("got a mutable reference to {}", mr ),
}
```

## 重载运算符
type Output = A 必须要有，输出结果

```rust
use std::ops::{Add, Sub};

#[derive(Copy, Clone)]
struct A(i32);

impl Add for A {
    type Output = A;
    fn add(self, rhs: A) -> A {
        A(self.0 + rhs.0)
    }
}

impl Sub for A {
    type Output = A;
    fn sub(self, rhs: A) -> A{
        A(self.0 - rhs.0)
    }
}

fn main() {
    let a1 = A(10i32);
    let a2 = A(5i32);
    let a3 = a1 + a2;
    println!("{}", (a3).0);
    let a4 = a1 - a2;
    println!("{}", (a4).0);
}
```
## use and crate
`use` bring a `trait` into package, or shorten the namespace.
`extern crate` import package.

``` rust
enum Status {
    Rich,
    Poor,
}

enum Work {
    Civilian,
    Soldier,
}

    // Explicitly `use` each name so they are available without
    // manual scoping.
    use Status::{Poor, Rich};
    // Automatically `use` each name inside `Work`.
    use Work::*;

    // Equivalent to `Status::Poor`.
    let status = Poor;
    // Equivalent to `Work::Civilian`.
    let work = Civilian;
```

## reference call
the `.` operator implicitly dereferences its left operand, if needed.
``` rust
let mut v = vec![1973, 1968];
v.sort();
(&mut v).sort(); // equivalent; much uglier
```
## fmt::Display and Debug
display format {}
the debug format {:?}

tuple element can be different types, list must be the same type.

## A common use for enums is to create a linked-list:
copy from [Testcase: linked-list](https://rustbyexample.com/custom_types/enum/testcase_linked_list.html)
``` rust
use List::*;

enum List {
    // Cons: Tuple struct that wraps an element and a pointer to the next node
    Cons(u32, Box<List>),
    // Nil: A node that signifies the end of the linked list
    Nil,
}

// Methods can be attached to an enum
impl List {
    // Create an empty list
    fn new() -> List {
        // `Nil` has type `List`
        Nil
    }

    // Consume a list, and return the same list with a new element at its front
    fn prepend(self, elem: u32) -> List {
        // `Cons` also has type List
        Cons(elem, Box::new(self))
    }

    // Return the length of the list
    fn len(&self) -> u32 {
        // `self` has to be matched, because the behavior of this method
        // depends on the variant of `self`
        // `self` has type `&List`, and `*self` has type `List`, matching on a
        // concrete type `T` is preferred over a match on a reference `&T`
        match *self {
            // Can't take ownership of the tail, because `self` is borrowed;
            // instead take a reference to the tail
            Cons(_, ref tail) => 1 + tail.len(),
            // Base Case: An empty list has zero length
            Nil => 0
        }
    }

    // Return representation of the list as a (heap allocated) string
    fn stringify(&self) -> String {
        match *self {
            Cons(head, ref tail) => {
                // `format!` is similar to `print!`, but returns a heap
                // allocated string instead of printing to the console
                format!("{}, {}", head, tail.stringify())
            },
            Nil => {
                format!("Nil")
            },
        }
    }
}

fn main() {
    // Create an empty linked list
    let mut list = List::new();

    // Append some elements
    list = list.prepend(1);
    list = list.prepend(2);
    list = list.prepend(3);

    // Show the final state of the list
    println!("linked list has length: {}", list.len());
    println!("{}", list.stringify());
}

```
The output is:

```
linked list has length: 3
3, 2, 1, Nil
```
The linked list is recursive, and I think this is not very good.
Please note that `&self`, `*self`, `ref tail`. This is a very good example to understand the reference and derefernce.
