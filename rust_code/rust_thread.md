# rust thread

## spawn

``` rust
use std::thread;

thread::spawn(move || {
    // some work here
});
```
copy from [Module std::thread](https://doc.rust-lang.org/std/thread/)

## Builder

``` rust
use std::thread;

let builder = thread::Builder::new();

let handler = builder.spawn(|| {
    // thread code
}).unwrap();

handler.join().unwrap();
```
copy from [Struct std::thread::Builder](https://doc.rust-lang.org/std/thread/struct.Builder.html)
