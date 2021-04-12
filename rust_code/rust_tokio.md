# rust tokio

## tutorial
``` shell
cargo install mini-redis
cargo new my-redis
cd my-redis
```
Add dependencies to `Cargo.toml`

```
tokio = { version = "0.3", features = ["full"] }
mini-redis = "0.3"
```

Write the code

``` rust
use mini_redis::{client, Result};

#[tokio::main]
pub async fn main() -> Result<()> {
    // Open a connection to the mini-redis address.
    let mut client = client::connect("127.0.0.1:6379").await?;

    // Set the key "hello" with value "world"
    client.set("hello", "world".into()).await?;

    // Get key "hello"
    let result = client.get("hello").await?;

    println!("got value from the server; result={:?}", result);

    Ok(())
}
```

run the mini-redis-server

``` shell
mini-redis-server
```

``` shell
cargo run
```
copy from [Hello Tokio](https://tokio.rs/tokio/tutorial/hello-tokio)

## code reading

[深入浅出Rust异步编程之Tokio](https://zhuanlan.zhihu.com/p/107820568)
[Rust 的异步函数与 Tokio.rs](https://zhuanlan.zhihu.com/p/244047486)


## rust Future trait

``` rust
pub trait Future {
    type Item;
    type Error;
    fn poll(&mut self) -> Poll<Self::Item, Self::Error>;
}
```


## Feature flags

```
full: Enables all Tokio public API features listed below except test-util.
rt: Enables tokio::spawn, the basic (current thread) scheduler, and non-scheduler utilities.
rt-multi-thread: Enables the heavier, multi-threaded, work-stealing scheduler.
io-util: Enables the IO based Ext traits.
io-std: Enable Stdout, Stdin and Stderr types.
net: Enables tokio::net types such as TcpStream, UnixStream and UdpSocket, as well as (on Unix-like systems) AsyncFd
time: Enables tokio::time types and allows the schedulers to enable the built in timer.
process: Enables tokio::process types.
macros: Enables #[tokio::main] and #[tokio::test] macros.
sync: Enables all tokio::sync types.
signal: Enables all tokio::signal types.
fs: Enables tokio::fs types.
test-util: Enables testing based infrastructure for the Tokio runtime.
Note: AsyncRead and AsyncWrite traits do not require any features and are always available.
```

## Modules

```
fs	fsAsynchronous file and standard stream adaptation.
io	Traits, helpers, and type definitions for asynchronous I/O functionality.
net	TCP/UDP/Unix bindings for tokio.
process	processAn implementation of asynchronous process management for Tokio.
runtime	rtThe Tokio runtime.
signal	signalAsynchronous signal handling for Tokio
stream	Due to the Stream trait’s inclusion in std landing later than Tokio’s 1.0 release, most of the Tokio stream utilities have been moved into the tokio-stream crate.
sync	syncSynchronization primitives for use in asynchronous contexts.
task	Asynchronous green-threads.
time	timeUtilities for tracking time.
```
