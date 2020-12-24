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


## current thread name

``` rust
use std::thread;
use std::time;

fn main() {
    let tid = thread::current();
    println!("{:?}", tid);
    thread::spawn(move || {
        thread::sleep(time::Duration::from_secs(1));
        println!("hello");
        tid.unpark(); // wake up
    });
    thread::park(); // sleep
    println!("world");
}
```
output:

``` shell
Thread { id: ThreadId(1), name: Some("main") }
hello
world
```
