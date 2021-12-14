# rust arc

## code definition

``` rust
pub struct Arc<T: ?Sized> {
    ptr: NonNull<ArcInner<T>>,
    phantom: PhantomData<ArcInner<T>>,
}

pub struct NonNull<T: ?Sized> {
    pointer: *const T,
}

pub struct PhantomData<T: ?Sized>;

// This is repr(C) to future-proof against possible field-reordering, which
// would interfere with otherwise safe [into|from]_raw() of transmutable
// inner types.
#[repr(C)]
struct ArcInner<T: ?Sized> {
    strong: atomic::AtomicUsize,

    // the value usize::MAX acts as a sentinel for temporarily "locking" the
    // ability to upgrade weak pointers or downgrade strong ones; this is used
    // to avoid races in `make_mut` and `get_mut`.
    weak: atomic::AtomicUsize,

    data: T,
}


pub fn new(data: T) -> Arc<T> {
        // Start the weak pointer count as 1 which is the weak pointer that's
        // held by all the strong pointers (kinda), see std/rc.rs for more info
        let x: Box<_> = box ArcInner {
            strong: atomic::AtomicUsize::new(1),
            weak: atomic::AtomicUsize::new(1),
            data,
        };
        Self::from_inner(Box::leak(x).into())
    }

impl<T: ?Sized> Arc<T> {
    fn from_inner(ptr: NonNull<ArcInner<T>>) -> Self {
        Self { ptr, phantom: PhantomData }
    }

    unsafe fn from_ptr(ptr: *mut ArcInner<T>) -> Self {
        unsafe { Self::from_inner(NonNull::new_unchecked(ptr)) }
    }
}

```

## rc definition

``` rust
pub struct Rc<T: ?Sized> {
    ptr: NonNull<RcBox<T>>,
    phantom: PhantomData<RcBox<T>>,
}

#[repr(C)]
struct RcBox<T: ?Sized> {
    strong: Cell<usize>,
    weak: Cell<usize>,
    value: T,
}
```

## Arc and Mutex code and enhancement

``` rust
type Db = Arc<Mutex<HashMap<String, Bytes>>>;
```
enhancement:

``` rust
type ShardedDb = Arc<Vec<Mutex<HashMap<String, Vec<u8>>>>>;

let shard = db[hash(key) % db.len()].lock().unwrap();
shard.insert(key, value);
```
copy from [Shared state](https://tokio.rs/tokio/tutorial/shared-state)

## Arc and RwLock example from quilkin

``` rust
// Tracks current sessions by their [`SessionKey`]
type SessionsMap = HashMap<SessionKey, Session>;
type Sessions = Arc<RwLock<SessionsMap>>;

#[derive(Clone)]
pub struct SessionManager(Sessions);

/// Contains arguments to process a received downstream packet, through the
/// filter chain and session pipeline.
struct ProcessDownstreamReceiveConfig {
    log: Logger,
    proxy_metrics: ProxyMetrics,
    session_metrics: SessionMetrics,
    cluster_manager: SharedClusterManager,
    filter_manager: SharedFilterManager,
    session_manager: SessionManager,
    session_ttl: Duration,
    send_packets: mpsc::Sender<Packet>,
}

    /// Send a packet received from `recv_addr` to an endpoint.
    async fn session_send_packet(
        packet: &[u8],
        recv_addr: SocketAddr,
        endpoint: &Endpoint,
        args: &ProcessDownstreamReceiveConfig,
    ) {
        let session_key = SessionKey {
            source: recv_addr,
            destination: endpoint.address,
        };

        // Grab a read lock and find the session.
        let guard = args.session_manager.get_sessions().await;
        if let Some(session) = guard.get(&session_key) {
            // If it exists then send the packet, we're done.
            Self::session_send_packet_helper(&args.log, session, packet, args.session_ttl).await
        } else {
            // If it does not exist, grab a write lock so that we can create it.
            //
            // NOTE: We must drop the lock guard to release the lock before
            // trying to acquire a write lock since these lock aren't reentrant,
            // otherwise we will deadlock with our self.
            drop(guard);

            // Grab a write lock.
            let mut guard = args.session_manager.get_sessions_mut().await;

            // Although we have the write lock now, check whether some other thread
            // managed to create the session in-between our dropping the read
            // lock and grabbing the write lock.
            if let Some(session) = guard.get(&session_key) {
                // If the session now exists then we have less work to do,
                // simply send the packet.
                Self::session_send_packet_helper(&args.log, session, packet, args.session_ttl)
                    .await;
            } else {
                // Otherwise, create the session and insert into the map.
                match Session::new(
                    &args.log,
                    args.session_metrics.clone(),
                    args.filter_manager.clone(),
                    session_key.source,
                    endpoint.clone(),
                    args.send_packets.clone(),
                    args.session_ttl,
                )
                .await
                {
                    Ok(session) => {
                        // Insert the session into the map and release the write lock
                        // immediately since we don't want to block other threads while we send
                        // the packet. Instead, re-acquire a read lock and send the packet.
                        guard.insert(session.key(), session);

                        // Release the write lock.
                        drop(guard);

                        // Grab a read lock to send the packet.
                        let guard = args.session_manager.get_sessions().await;
                        if let Some(session) = guard.get(&session_key) {
                            Self::session_send_packet_helper(
                                &args.log,
                                session,
                                packet,
                                args.session_ttl,
                            )
                            .await;
                        } else {
                            warn!(
                                args.log,
                                "Could not find session";
                                "key" => format!("({}:{})", session_key.source.to_string(), session_key.destination.to_string())
                            )
                        }
                    }
                    Err(err) => {
                        error!(args.log, "Failed to ensure session exists"; "error" => %err);
                    }
                }
            }
        }
    }
```
Take more care with the `drop` function.

## &*pointer meaning

``` rust
use std::sync::{Arc, Mutex, Condvar};
use std::thread;

let pair = Arc::new((Mutex::new(false), Condvar::new()));
let pair2 = pair.clone();

// Inside of our lock, spawn a new thread, and then wait for it to start.
thread::spawn(move|| {
    let (lock, cvar) = &*pair2;
    let mut started = lock.lock().unwrap();
    *started = true;
    // We notify the condvar that the value has changed.
    cvar.notify_one();
});

// Wait for the thread to start up.
let (lock, cvar) = &*pair;
let mut started = lock.lock().unwrap();
while !*started {
    started = cvar.wait(started).unwrap();
}
```
And the meaning of the &*pair is :

```
The * operator turns the Arc<T> into T. The & operator borrows that T into &T.

So when we put them together, &*pair borrows the Arc<T> into &T.

Another way of writing that code would be:

let (lock, cvar) = pair2.deref();

Indeed, the original &*pair2 actually means &*pair2.deref() – the * forces the compiler to insert a .deref() call, and it's that method which performs the actual conversion.

```
copy from [Understanding &* to access a Rust Arc](https://stackoverflow.com/questions/62651479/understanding-to-access-a-rust-arc)
