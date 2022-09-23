# rust loop select example

## object-orientd loop
code copy from quilkin

``` rust
/// Server is the UDP server main implementation
pub struct Server {
    // We use pub(super) to limit instantiation only to the Builder.
    pub(super) log: Logger,
    pub(super) config: Arc<ValidatedConfig>,
    // Admin may be turned off, primarily for testing.
    pub(super) admin: Option<Admin>,
    pub(super) metrics: Arc<Metrics>,
    pub(super) proxy_metrics: ProxyMetrics,
    pub(super) session_metrics: SessionMetrics,
    pub(super) filter_registry: FilterRegistry,
}


impl Server {
    pub async fn run(self, mut shutdown_rx: watch::Receiver<()>) -> Result<()> {
        self.log_config();

        if let Some(admin) = &self.admin {
            admin.run(shutdown_rx.clone());
        }

        let socket = Arc::new(Server::bind(self.config.proxy.port).await?);
        let session_manager = SessionManager::new(self.log.clone(), shutdown_rx.clone());
        let (send_packets, receive_packets) = mpsc::channel::<Packet>(1024);

        let session_ttl = Duration::from_secs(SESSION_TIMEOUT_SECONDS);

        let (cluster_manager, filter_manager) =
            self.create_resource_managers(shutdown_rx.clone()).await?;
        self.run_receive_packet(socket.clone(), receive_packets);
        let recv_loop = self.run_recv_from(RunRecvFromArgs {
            cluster_manager,
            filter_manager,
            socket,
            session_manager,
            session_ttl,
            send_packets,
            shutdown_rx: shutdown_rx.clone(),
        });

        tokio::select! {
            join_result = recv_loop => {
                join_result
                    .map_err(|join_err| Error::RecvLoop(format!("{}", join_err)))
                    .and_then(|inner| inner.map_err(Error::RecvLoop))
            }
            _ = shutdown_rx.changed() => {
                Ok(())
            }
        }
    }


    fn run_recv_from(&self, args: RunRecvFromArgs) -> JoinHandle<StdResult<(), String>> {
        let session_manager = args.session_manager;
        let log = self.log.clone();
        let proxy_metrics = self.proxy_metrics.clone();
        let session_metrics = self.session_metrics.clone();

        // The number of worker tasks to spawn. Each task gets a dedicated queue to
        // consume packets off.
        let num_workers = num_cpus::get();

        // Contains channel Senders for each worker task.
        let mut packet_txs = vec![];
        // Contains config for each worker task.
        let mut worker_configs = vec![];
        for worker_id in 0..num_workers {
            let (packet_tx, packet_rx) = mpsc::channel(num_workers);
            packet_txs.push(packet_tx);
            worker_configs.push(DownstreamReceiveWorkerConfig {
                worker_id,
                packet_rx,
                shutdown_rx: args.shutdown_rx.clone(),
                receive_config: ProcessDownstreamReceiveConfig {
                    log: log.clone(),
                    proxy_metrics: proxy_metrics.clone(),
                    session_metrics: session_metrics.clone(),
                    cluster_manager: args.cluster_manager.clone(),
                    filter_manager: args.filter_manager.clone(),
                    session_manager: session_manager.clone(),
                    session_ttl: args.session_ttl,
                    send_packets: args.send_packets.clone(),
                },
            })
        }

        // Start the worker tasks that pick up received packets from their queue
        // and processes them.
        Self::spawn_downstream_receive_workers(log.clone(), worker_configs);

        // Start the background task to receive downstream packets from the socket
        // and place them onto the worker tasks' queue for processing.
        let socket = args.socket;
        tokio::spawn(async move {
            // Index to round-robin over workers to process packets.
            let mut next_worker = 0;
            let num_workers = num_workers;

            // Initialize a buffer for the UDP packet. We use the maximum size of a UDP
            // packet, which is the maximum value of 16 a bit integer.
            let mut buf = vec![0; 1 << 16];
            loop {
                match socket.recv_from(&mut buf).await {
                    Ok((size, recv_addr)) => {
                        let packet_tx = &mut packet_txs[next_worker % num_workers];
                        next_worker += 1;

                        if packet_tx
                            .send((recv_addr, (&buf[..size]).to_vec()))
                            .await
                            .is_err()
                        {
                            // We cannot recover from this error since
                            // it implies that the receiver has been dropped.
                            let reason =
                                "Failed to send received packet over channel to worker".into();
                            error!(log, "{}", reason);
                            return Err(reason);
                        }
                    }
                    err => {
                        // Socket error, we cannot recover from this so return an error instead.
                        error!(log, "Error processing receive socket"; "error" => #?err);
                        return Err(format!("error processing receive socket: {:?}", err));
                    }
                }
            }
        })
    }
```

in the runner.rs

``` rust
/// Start and run a proxy. Any passed in [`FilterFactory`]s are included
/// alongside the default filter factories.
pub async fn run_with_config(
    base_log: slog::Logger,
    config: Arc<Config>,
    filter_factories: impl IntoIterator<Item = DynFilterFactory>,
) -> Result<(), Error> {
    let log = base_log.new(o!("source" => "run"));
    let server = Builder::from(config)
        .with_log(base_log)
        .with_filter_registry(FilterRegistry::new(FilterSet::default_with(
            &log,
            filter_factories.into_iter(),
        )))
        .validate()?
        .build();

    let (shutdown_tx, shutdown_rx) = watch::channel::<()>(());
    tokio::spawn(async move {
        // Don't unwrap in order to ensure that we execute
        // any subsequent shutdown tasks.
        signal::ctrl_c().await.ok();
        shutdown_tx.send(()).ok();
    });

    if let Err(err) = server.run(shutdown_rx).await {
        info!(log, "Shutting down with error"; "error" => %err);
        Err(Error::from(err))
    } else {
        info!(log, "Shutting down");
        Ok(())
    }
}
```

## normal loop

``` rust
    fn spawn_updater(
        log: Logger,
        filter_manager: SharedFilterManager,
        mut filter_chain_updates_rx: mpsc::Receiver<Arc<FilterChain>>,
        mut shutdown_rx: watch::Receiver<()>,
    ) {
        tokio::spawn(async move {
            loop {
                tokio::select! {
                    update = filter_chain_updates_rx.recv() => {
                        match update {
                            Some(filter_chain) => {
                                debug!(log, "Received a filter chain update.");
                                filter_manager.write().update(filter_chain);
                            }
                            None => {
                                warn!(log, "Exiting filter chain update receive loop because the sender dropped the channel.");
                                return;
                            }
                        }
                    }
                    _ = shutdown_rx.changed() => {
                        debug!(log, "Exiting filter chain update receive loop because a shutdown signal was received.");
                        return;
                    },
                }
            }
        });
    }
```


## session_manager
copy from session_manager.rs
``` rust
// Tracks current sessions by their [`SessionKey`]
type SessionsMap = HashMap<SessionKey, Session>;
type Sessions = Arc<RwLock<SessionsMap>>;
pub struct SessionManager(Sessions);

impl SessionManager {
    pub fn new(log: Logger, shutdown_rx: watch::Receiver<()>) -> Self {
        let poll_interval = Duration::from_secs(SESSION_EXPIRY_POLL_INTERVAL);
        let sessions: Sessions = Arc::new(RwLock::new(HashMap::new()));

        Self::run_prune_sessions(log.clone(), sessions.clone(), poll_interval, shutdown_rx);

        Self(sessions)
    }

    /// run_prune_sessions starts the timer for pruning sessions and runs prune_sessions every
    /// SESSION_TIMEOUT_SECONDS, via a tokio::spawn, i.e. it's non-blocking.
    /// Pruning will occur ~ every interval period. So the timeout expiration may sometimes
    /// exceed the expected, but we don't have to write lock the Sessions map as often to clean up.
    fn run_prune_sessions(
        log: Logger,
        mut sessions: Sessions,
        poll_interval: Duration,
        mut shutdown_rx: watch::Receiver<()>,
    ) {
        let mut interval = tokio::time::interval(poll_interval);

        tokio::spawn(async move {
            loop {
                tokio::select! {
                    _ = shutdown_rx.changed() => {
                        debug!(log, "Exiting Prune Sessions due to shutdown signal.");
                        break;
                    }
                    _ = interval.tick() => {
                        debug!(log, "Attempting to Prune Sessions");
                        Self::prune_sessions(&log, &mut sessions).await;

                    }
                }
            }
        });
    }
}
```


## session

``` rust
/// Session encapsulates a UDP stream session
pub struct Session {
    log: Logger,
    metrics: Metrics,
    filter_manager: SharedFilterManager,
    /// created_at is time at which the session was created
    created_at: Instant,
    socket: Arc<UdpSocket>,
    /// dest is where to send data to
    dest: Endpoint,
    /// from is the original sender
    from: SocketAddr,
    /// The time at which the session is considered expired and can be removed.
    expiration: Arc<AtomicU64>,
    /// a channel to broadcast on if we are shutting down this Session
    shutdown_tx: watch::Sender<()>,
}


/ A (source, destination) address pair that uniquely identifies a session.
#[derive(Clone, Eq, Hash, PartialEq, Debug, PartialOrd, Ord)]
pub struct SessionKey {
    pub source: SocketAddr,
    pub destination: SocketAddr,
}

impl Session {
    /// new creates a new Session, and starts the process of receiving udp sockets
    /// from its ephemeral port from endpoint(s)
    pub async fn new(
        base: &Logger,
        metrics: Metrics,
        filter_manager: SharedFilterManager,
        from: SocketAddr,
        dest: Endpoint,
        sender: mpsc::Sender<Packet>,
        ttl: Duration,
    ) -> Result<Self> {
        let log = base
            .new(o!("source" => "proxy::Session", "from" => from, "dest_address" => dest.address));
        let addr = SocketAddrV4::new(Ipv4Addr::new(0, 0, 0, 0), 0);
        let socket = Arc::new(UdpSocket::bind(addr).await.map_err(Error::BindUdpSocket)?);
        let (shutdown_tx, shutdown_rx) = watch::channel::<()>(());

        let expiration = Arc::new(AtomicU64::new(0));
        Self::do_update_expiration(&expiration, ttl)?;

        let s = Session {
            metrics,
            log,
            filter_manager,
            socket: socket.clone(),
            from,
            dest,
            created_at: Instant::now(),
            expiration,
            shutdown_tx,
        };
        debug!(s.log, "Session created");

        s.metrics.sessions_total.inc();
        s.metrics.active_sessions.inc();
        s.run(ttl, socket, sender, shutdown_rx);
        Ok(s)
    }

    /// run starts processing received udp packets on its UdpSocket
    fn run(
        &self,
        ttl: Duration,
        socket: Arc<UdpSocket>,
        mut sender: mpsc::Sender<Packet>,
        mut shutdown_rx: watch::Receiver<()>,
    ) {
        let log = self.log.clone();
        let from = self.from;
        let expiration = self.expiration.clone();
        let filter_manager = self.filter_manager.clone();
        let endpoint = self.dest.clone();
        let metrics = self.metrics.clone();
        tokio::spawn(async move {
            let mut buf: Vec<u8> = vec![0; 65535];
            loop {
                debug!(log, "Awaiting incoming packet");
                select! {
                    received = socket.recv_from(&mut buf) => {
                        match received {
                            Err(err) => {
                                metrics.rx_errors_total.inc();
                                error!(log, "Error receiving packet"; "error" => %err);
                            },
                            Ok((size, recv_addr)) => {
                                metrics.rx_bytes_total.inc_by(size as u64);
                                metrics.rx_packets_total.inc();
                                Session::process_recv_packet(
                                    &log,
                                    &metrics,
                                    &mut sender,
                                    &expiration,
                                    ttl,
                                    ReceivedPacketContext {
                                        filter_manager: filter_manager.clone(),
                                        packet: &buf[..size],
                                        endpoint: &endpoint,
                                        from: recv_addr,
                                        to: from,
                                    }).await
                            }
                        };
                    }
                    _ = shutdown_rx.changed() => {
                        debug!(log, "Closing Session");
                        return;
                    }
                };
            }
        });
    }
}
```

## ClusterManager

``` rust
    /// Spawns a task to run a loop that receives cluster updates
    /// and updates the ClusterManager's state in turn.
    fn spawn_updater(
        log: Logger,
        metrics: Metrics,
        cluster_manager: Arc<RwLock<ClusterManager>>,
        mut cluster_updates_rx: mpsc::Receiver<ClusterUpdate>,
        mut shutdown_rx: watch::Receiver<()>,
    ) {
        tokio::spawn(async move {
            loop {
                tokio::select! {
                    update = cluster_updates_rx.recv() => {
                        match update {
                            Some(update) => {
                                debug!(log, "Received a cluster update.");
                                cluster_manager.write().update(Self::process_cluster_update(&metrics, update));
                            }
                            None => {
                                warn!(log, "Exiting cluster update receive loop because the sender dropped the channel.");
                                return;
                            }
                        }
                    }
                    _ = shutdown_rx.changed() => {
                        debug!(log, "Exiting cluster update receive loop because a shutdown signal was received.");
                        return;
                    },
                }
            }
        });
    }
```

## reactor

``` rust
loop {
    poll.poll(&events, timeout);
    for event in events.iter() {
        if (event.is_readable()) {
            for waker in event.readers.wakers {
                waker.wake();
            }
        }
        if (event.is_writeable()) {
            for waker in event.writers.wakers {
                waker.wake();
            }
        }
    }
}
```
copy from [Rust异步浅谈](https://leaxoy.github.io/2020/03/rust-async-runtime/)


## loop example
see [Is it possible to take ownership of data back from a for loop once the loop completes](https://users.rust-lang.org/t/is-it-possible-to-take-ownership-of-data-back-from-a-for-loop-once-the-loop-completes/3363)
see [Rust borrowing rules in `for...in` loop](https://stackoverflow.com/questions/60461475/rust-borrowing-rules-in-for-in-loop)
see [Rust: Looping on a member variable without mutably borrowing self](http://blog.ssokolow.com/archives/2017/06/23/rust-looping-on-a-member-variable-without-mutably-borrowing-self/)
