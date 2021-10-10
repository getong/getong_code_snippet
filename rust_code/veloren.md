# veloren

## quic protocol

## Server

``` rust
pub struct Server {
    state: State,
    world: Arc<World>,
    index: IndexOwned,
    map: WorldMapMsg,

    connection_handler: ConnectionHandler,

    runtime: Arc<Runtime>,

    metrics_shutdown: Arc<Notify>,
    database_settings: Arc<RwLock<DatabaseSettings>>,
    disconnect_all_clients_requested: bool,
}
```
data  persistence, setting, metrics, ban words.


## server-cli has the main.rs file

## veloren book
[Veloren: An Owner's Manual](https://book.veloren.net/introduction/index.html)

## tokio runtime

``` rust
    // We don't need that many threads in the async pool, at least 2 but generally
    // 25% of all available will do
    // TODO: evaluate std::thread::available_concurrency as a num_cpus replacement
    let runtime = Arc::new(
        tokio::runtime::Builder::new_multi_thread()
            .enable_all()
            .worker_threads((num_cpus::get() / 4).max(MIN_RECOMMENDED_TOKIO_THREADS))
            .thread_name_fn(|| {
                static ATOMIC_ID: AtomicUsize = AtomicUsize::new(0);
                let id = ATOMIC_ID.fetch_add(1, Ordering::SeqCst);
                format!("tokio-server-{}", id)
            })
            .build()
            .unwrap(),
    );
```

Then use the `runtime` :

``` rust
ArgvCommand::Shared(SharedCommand::Admin { command }) => {
                let login_provider = server::login_provider::LoginProvider::new(
                    server_settings.auth_server_address,
                    runtime,
                );
```
and this:

``` rust
    // Create server
    let mut server = Server::new(
        server_settings,
        editable_settings,
        database_settings,
        &server_data_dir,
        runtime,
    )
    .expect("Failed to create server instance!");
```
