# rust redis

``` rust
 let redis_uri = env::var("REDIS_URI").expect("REDIS_URI env var should be specified");
    let redis_client = redis::create_client(redis_uri)
        .await
        .expect("Can't create Redis client");
    let redis_connection_manager = redis_client
        .get_tokio_connection_manager()
        .await
        .expect("Can't create Redis connection manager");

    let planet_service = Arc::new(PlanetService::new(
        mongodb_client,
        redis_client,
        redis_connection_manager.clone(),
    ));
    let rate_limiting_service = Arc::new(RateLimitingService::new(redis_connection_manager));

```

copy from [Getting started with MongoDB and Redis in Rust](https://romankudryashov.com/blog/2021/06/mongodb-redis-rust/)
also see [MongoDB + Redis demo](https://github.com/rkudryashov/exploring-rust-ecosystem/tree/master/mongodb-redis)
