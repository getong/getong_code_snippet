# rust channel

## ConnectionManager example

``` rust
/// Implements a concept of connections on top of datagram socket.
/// Connection capabilities depends on what is an actual `Connection` type.
/// Connection type also defines a type of sending and receiving events.
#[derive(Debug)]
pub struct ConnectionManager<TSocket: DatagramSocket, TConnection: Connection> {
    connections: HashMap<SocketAddr, TConnection>,
    receive_buffer: Vec<u8>,
    user_event_receiver: Receiver<TConnection::SendEvent>,
    messenger: SocketEventSenderAndConfig<TSocket, TConnection::ReceiveEvent>,
    event_receiver: Receiver<TConnection::ReceiveEvent>,
    user_event_sender: Sender<TConnection::SendEvent>,
    max_unestablished_connections: u16,
}

impl<TSocket: DatagramSocket, TConnection: Connection> ConnectionManager<TSocket, TConnection> {
    /// Creates an instance of `ConnectionManager` by passing a socket and config.
    pub fn new(socket: TSocket, config: Config) -> Self {
        let (event_sender, event_receiver) = unbounded();
        let (user_event_sender, user_event_receiver) = unbounded();
        let max_unestablished_connections = config.max_unestablished_connections;

        ConnectionManager {
            receive_buffer: vec![0; config.receive_buffer_max_size],
            connections: Default::default(),
            user_event_receiver,
            messenger: SocketEventSenderAndConfig::new(config, socket, event_sender),
            user_event_sender,
            event_receiver,
            max_unestablished_connections,
        }
    }
    ...
}
```
copy from laminar/src/net/connection_manager.rs

## mpmc
[flume](https://github.com/zesterer/flume)
[crossbeam-channel](https://crates.io/crates/crossbeam-channel)

flume might be better.
