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
```
