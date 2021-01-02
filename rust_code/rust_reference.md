# rust reference

## structs containning references
Whenever a reference type appears inside another type's definition, you must write out its lifetime. You can write this:

``` rust
struct S {
	r: &'static i32
}

struct S<'a> {
	r: &'a i32
}
```
## coercion
强制隐式转换，是Rust中仅有的类型隐式转换。

## cow

``` rust
use std::borrow::Cow;

let mut lst = Cow::from(&[1,2,3]);
```


## ref and & operator, match

``` rust

fn main() {
    let x = &false;
    print_type_name_of(x);

    let &x = &false;
    print_type_name_of(x);

    let ref x = &false;
    print_type_name_of(x);
}

fn print_type_name_of<T>(_: T) {
    println!("{}", unsafe { std::any::type_name::<T>() })
}

```
output :

``` rust
&bool
bool
&&bool
```

`match` and `ref` usage:

``` rust
enum Favour<'a> {
    Nor(u32),
    NorRef(u32),
    Ref(&'a u32),
    RefRef(&'a u32),
}

fn config(data: &u32) {
    println!("log data: {}", data);
}

// fn con(data: ref u32){  //expected type, found keyword `ref`
//     println!("log data: {}", data);
// }

fn log(fav: Favour) {
    match fav {
        Favour::Nor(data) => {
            config(&data);
            print_type_name_of(data);
        },
        Favour::NorRef(ref data) => {
            config(data);
            print_type_name_of(data);
        },
        Favour::Ref(data) => {
            config(data);
            print_type_name_of(data);
        },
        Favour::RefRef(ref data) => {
            config(data);
            print_type_name_of(data);
        }
    }
}

fn print_type_name_of<T>(_: T) {
    println!("{}", unsafe { std::any::type_name::<T>() })
}

fn main() {
    log(Favour::Nor(1));
    log(Favour::Ref(&2));
    log(Favour::NorRef(3));
    log(Favour::RefRef(&4));
}


log data: 1
u32
log data: 2
&u32
log data: 3
&u32
log data: 4
&&u32
```

copy from [理解 Rust 引用和借用](https://zhuanlan.zhihu.com/p/59998584)


## for reference

``` rust
for (i, item) in bytes.iter().enumerate() {
    if *item == b' ' {
        return i;
    }
}

for (i, &item) in bytes.iter().enumerate() {
    if item == b' ' {
        return i;
    }
}

for (i, item) in bytes.iter().enumerate() {
    if item == &b' ' {
        return i;
    }
}
```
The three styles code are the same.
