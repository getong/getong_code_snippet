# rust lifetime

[Understanding Rust Lifetimes](https://medium.com/nearprotocol/understanding-rust-lifetimes-e813bcd405fa)
[【译】深入理解Rust中的生命周期](https://mp.weixin.qq.com/s/PdM1Q6eLWie5opfzjXavRg)

## elision rule

```
Each elided lifetime in input position becomes a distinct lifetime parameter. This is the current behavior for fn definitions.

If there is exactly one input lifetime position (elided or not), that lifetime is assigned to all elided output lifetimes.

If there are multiple input lifetime positions, but one of them is &self or &mut self, the lifetime of self is assigned to all elided output lifetimes.

Otherwise, it is an error to elide an output lifetime.
```
copy from [lifetime-elision](https://rust-lang.github.io/rfcs/0141-lifetime-elision.html)
translation:

```
当前Rust在以下三种情况下可以省略生命周期声明：

函数的每个参数将会赋予各自的生命周期。例如fn foo(x: &i32)将相当于为fn foo<'a>(x: &'a i32)，fn foo(x: &i32, y: &i32)相当于fn foo<'a, 'b>(x: &'a i32, y: &'b i32)，以此类推。
如果输入参数只有一个生命周期参数，那个这个生命周期参数将会被赋予所有输入值。例如fn foo(x: &i32) -> &i32相当于fn foo<'a>(x: &'a i32) -> &'a i32。
在struct的impl语句中，如果有多个输入参数，但是输入参数中有&self或者&mut self，那么self的生命周期将会被赋予所有的书参数。这条规则对于编写struct方法是非常有利的。

```
copy from [Rust生命周期详解](https://zhuanlan.zhihu.com/p/93193353)

## drop in another thread

``` rust
use std::collections::HashMap;
use std::thread;
use std::time::Instant;
const NUM_ELEMENTS: usize = 1000000;
type HeavyThings = HashMap<usize, Vec<usize>>;
fn main() {
    let heavy_things_1 = make_heavy_things();
    let heavy_things_2 = make_heavy_things();
    let len =log_time("drop in another thread", || {
        fn_that_drops_heavy_things_in_another_thread(heavy_things_2)
    });
    assert_eq!(len, NUM_ELEMENTS);
    let len = log_time("drop in this thread", || {
        fn_that_drops_heavy_things(heavy_things_1)
    });
    assert_eq!(len, NUM_ELEMENTS);
}
fn make_heavy_things() -> HeavyThings {
    (1..=NUM_ELEMENTS).map(|v| (v, vec![v])).collect()
}
fn fn_that_drops_heavy_things(things: HeavyThings) -> usize {
    things.len()
}
fn fn_that_drops_heavy_things_in_another_thread(things: HeavyThings) -> usize {
    let len = things.len();
    thread::spawn(move || drop(things));
    len
}
fn log_time<T, F: FnOnce() -> T>(name: &str, f: F) -> T {
    let time = Instant::now();
    let result = f();
    println!("{} {:?}", name, time.elapsed());
    result
}
```
output:

```
drop in another thread 52.518µs
drop in this thread 548.50784ms
```
copy from [Rust: Dropping heavy things in another thread can make your code 10000 times faster](https://abramov.io/rust-dropping-things-in-another-thread/)

## const
The compiler cam embed the result of that computation as a compile-time constant. This is similar to C++ constexpr.
``` rust
const fn mono_to_rgba(level: u8) -> Color {
    Color {
    red: level,
    green: level,
    blue: level,
    alpha: 0xFF
    }
}
```
