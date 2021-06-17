# rust mutex

## scope and lock

This not work!

``` rust
use std::sync::Mutex;

async fn increment_and_do_stuff(mutex: &Mutex<i32>) {
    let mut lock = mutex.lock().unwrap();
    *lock += 1;

    do_something_async().await;
} // lock goes out of scope here

```
This fails too.

``` rust
use std::sync::Mutex;

// This fails too.
async fn increment_and_do_stuff(mutex: &Mutex<i32>) {
    let mut lock = mutex.lock().unwrap();
    *lock += 1;
    drop(lock);

    do_something_async().await;
}
```
This works:

``` rust
async fn increment_and_do_stuff(mutex: &Mutex<i32>) {
    {
        let mut lock = mutex.lock().unwrap();
        *lock += 1;
    } // lock goes out of scope here

    do_something_async().await;
}
```
