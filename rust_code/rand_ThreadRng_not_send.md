# rand ThreadRng does not implement send

## How to generate random numbers in async rust?
see [How to generate random numbers in async rust?](https://stackoverflow.com/questions/67443847/how-to-generate-random-numbers-in-async-rust)

## code can not write like below:

``` rust
    let (tx1, mut rx1) = mpsc::channel(32);

    tokio::spawn(async move {
        let mut i = 0;
        let mut rng = rand::thread_rng();
        tokio::pin!(rng);

        loop {
            let rand_sec = rng.gen_range(1u64..3u64);
            let _ = sleep(Duration::from_secs(rand_sec)).await;
            tx1.send(i).await.unwrap();
            i += 1;
        }
    });
```


## code change like this

``` rust
async fn random_number() -> u64 {
    let mut rng = rand::thread_rng();
    rng.gen_range(1u64..3u64)
}

    let (tx1, mut rx1) = mpsc::channel(32);

    tokio::spawn(async move {
        let mut i = 0;

        loop {
            let rand_sec = random_number().await;
            let _ = sleep(Duration::from_secs(rand_sec)).await;
            tx1.send(i).await.unwrap();
            i += 1;
        }
    });
```

## use getrandom crate
see [How can I get secure random bytes in a actix route, for password hashing?](https://users.rust-lang.org/t/how-can-i-get-secure-random-bytes-in-a-actix-route-for-password-hashing/50076)
