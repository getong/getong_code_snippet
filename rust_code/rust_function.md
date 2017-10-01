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
