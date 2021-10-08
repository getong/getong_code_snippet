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
