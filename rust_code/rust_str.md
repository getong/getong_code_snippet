# rust str

&str的类型不止一种。我们有。

字符串: 当你写let my_str = "I am a &str"的时候，你就会产生这些字符。它们在整个程序中持续存在，因为它们是直接写进二进制中的，它们的类型是 &'static str。'的意思是它的生命期，字符串字元有一个叫static的生命期。
借用str。这是常规的 &str 形式，没有 static 生命期。如果你创建了一个String，并得到了它的引用，当你需要它时，Rust会把它转换为&str。比如说

``` rust
fn prints_str(my_str: &str) { // it can use &String like a &str
    println!("{}", my_str);
}

fn main() {
    let my_string = String::from("I am a string");
    prints_str(&my_string); // we give prints_str a &String
}
```

copy from [&str的类型](https://kumakichi.github.io/easy_rust_chs/Chapter_39.html)
