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
