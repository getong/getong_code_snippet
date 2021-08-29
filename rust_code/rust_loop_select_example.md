# rust loop select example

## quilkin
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
