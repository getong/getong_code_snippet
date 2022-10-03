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

## Thread Pool detail

threadpool usage
``` rust
fn main() {
  let pool = ThreadPool::new(4);
  for _ in 0..10 {
    pool.execute(|| {
      thread::sleep(Duration::from_millis(1000));
      println!(
        "Work in thread = {}",
        thread::current().name().unwrap()
      );
    });
  }
}


```

threadpool detail:

``` rust
pub struct ThreadPool {
  workers: Vec<Worker>,
  sender: mpsc::Sender<Message>,
}

struct Worker {
  thread: JoinHandler<()>
}

enum Message {
  RunTask(BoxedTask),
  Terminate,
}

type Task = dyn FnOnce() + Send + 'static;
type BoxedTask = Box<Task>;

type SharedReceiver = Arc<Mutex<mpsc::Receiver<Message>>>;

impl Worker {
  fn new(id: usize, receiver: SharedReceiver) -> Self {
    let builder = thread::Builder::new().name(
      format!("worker-thread-{}", id)
    );

    let thread = builder
      .spawn(move || loop {
        let message = receiver.lock().unwrap().recv().unwrap();
        match message {
          Message::RunTask(task) => task(),
          Message::Terminate => {
            break;
          }
        }
      })
      .unwrap();
    Worker { thread }
  }
}

impl ThreadPool {
  pub fn new(num_threads: usize) -> Self {
    let mut workers = Vec::with_capacity(num_threads);
    let (sender, receiver) = mpsc::channel();
    let receiver: SharedReceiver = Arc::new(Mutex::new(receiver));
    for id in 0..num_threads {
      let receiver = Arc::clone(&receiver);
      let worker = Worker::new(id, receiver);
      workers.push(worker);
    }
    ThreadPool { workers, sender }
  }
}

pub fn execute<F>(&self, f: F)
where
  F: FnOnce() + Send + 'static,
{
  self.sender.send(Message::RunTask(Box::new(f))).unwrap();
}

pub fn spawn<F, T>(f: F) -> JoinHandle<T>
where
  F: FnOnce() -> T,
  F: Send + 'static,
  T: Send + 'static,
{
  Builder::new().spawn(f).expect("failed to spawn thread")
}

struct TaskHandle<T> {
  receiver: mpsc::Receiver<T>
}

impl<T> TaskHandle<T> {
  pub fn new(receiver: mpsc::Receiver<T>) -> Self {
    TaskHandle { receiver }
  }
  pub fn join(&self) -> Result<T, ()> {
    Ok(self.receiver.recv().unwrap())
  }
}

type Task = dyn FnOnce() + Send + 'static;
type BoxedTask = Box<Task>;

let task = move || {
  sender.send(f()).unwrap();
};


pub fn spawn<F, T>(&self, f: F) -> TaskHandle<T>
where
  F: FnOnce() -> T + Send + 'static,
  T: Send + 'static,
{
  let (sender, receiver) = mpsc::sync_channel::<T>(0);
  let task = move || {
    sender.send(f()).unwrap();
  };
  self.sender.send(Message::RunTask(Box::new(task))).unwrap();

  TaskHandle::new(receiver)
}

```
copy from [Async Programming in Rust — Part 1: Threads and Channels](https://medium.com/@KevinBGreene/async-programming-in-rust-part-1-threads-and-channels-736f8c87b04e)
