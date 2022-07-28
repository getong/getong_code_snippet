# tokio runtime

## tokio:spawn

``` rust
tokio::spawn(async move {
    for port in MOST_COMMON_PORTS_100 {
        let _ = input_tx.send(*port).await;
    }
});
```
like go code:

``` go
go doSomething()
```

spawn_blocking

``` rust
let is_code_valid = spawn_blocking(move || crypto::verify_password(&code, &code_hash)).await?;
```
copy from [Async Rust: What is a runtime? Here is how tokio works under the hood](https://kerkour.com/rust-async-await-what-is-a-runtime)
