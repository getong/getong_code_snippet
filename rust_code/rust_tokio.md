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
