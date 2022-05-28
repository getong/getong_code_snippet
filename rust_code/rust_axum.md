# rust axum

## handler

``` rust
pub struct Core {
    public_url: Url,
    secret_key: String,
    storage: Storage,
    client: SomeThirdPartyClient,
}

impl Core {
    pub async fn create_something(
        &self,
        new_something: NewSomething,
    ) -> Result<Something, BusinessErr> {
    ...
}
```
Core can be provided to handlers using Extension Layer mechanism like this:

``` rust

...
let shared_core = Arc::new(core);
...
let app = Router::new()
    .route("/something", post(post_something))
    ...
    .layer(AddExtensionLayer::new(shared_core));
```
Which in a handler can be declared in parameter list using extension extractor:

``` rust
async fn post_something(
    Extension(core): Extension<Arc<Core>>,
    Json(new_something): Json<NewSomething>,
) -> impl IntoResponse {
    core
        .create_something(new_something)
        .await
}
```
copy from [How to design the test-friendly application with Rust/Axum?](https://stackoverflow.com/questions/69415050/how-to-design-the-test-friendly-application-with-rust-axum)

also see [realworld-axum-sqlx](https://github.com/launchbadge/realworld-axum-sqlx)
