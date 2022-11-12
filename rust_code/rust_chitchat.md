# rust chitchat

## chitchat structure

``` rust
pub struct Chitchat {
    config: ChitchatConfig,
    cluster_state: ClusterState,
    heartbeat: u64,
    /// The failure detector instance.
    failure_detector: FailureDetector,
    /// A notification channel (sender) for sending live nodes change feed.
    ready_nodes_watcher_tx: watch::Sender<HashSet<NodeId>>,
    /// A notification channel (receiver) for receiving `ready` nodes change feed.
    ready_nodes_watcher_rx: watch::Receiver<HashSet<NodeId>>,
}


#[derive(Debug)]
pub(crate) struct ClusterState {
    pub(crate) node_states: BTreeMap<NodeId, NodeState>,
    seed_addrs: watch::Receiver<HashSet<SocketAddr>>,
}

#[derive(Clone, Hash, Eq, PartialEq, PartialOrd, Ord, Debug, Serialize, Deserialize)]
pub struct NodeId {
    // The unique identifier of this node in the cluster.
    pub id: String,
    // The SocketAddr other peers should use to communicate.
    pub gossip_public_address: SocketAddr,
}

#[derive(Clone, Serialize, Deserialize, Debug)]
pub struct NodeState {
    pub(crate) key_values: BTreeMap<String, VersionedValue>,
    #[serde(skip)]
    #[serde(default = "Instant::now")]
    last_heartbeat: Instant,
    pub(crate) max_version: u64,
}
```

## set with version

``` rust
impl NodeState {
    /// Returns an iterator over the version values that are older than `floor_version`.
    fn iter_stale_key_values(
        &self,
        floor_version: u64,
    ) -> impl Iterator<Item = (&str, &VersionedValue)> {
        // TODO optimize by checking the max version.
        self.key_values
            .iter()
            .filter(move |&(_key, versioned_value)| versioned_value.version > floor_version)
            .map(|(key, record)| (key.as_str(), record))
    }

    pub fn get(&self, key: &str) -> Option<&str> {
        self.get_versioned(key)
            .map(|versioned_value| versioned_value.value.as_str())
    }

    pub fn get_versioned(&self, key: &str) -> Option<&VersionedValue> {
        self.key_values.get(key)
    }

    /// Sets a new value for a given key.
    ///
    /// Setting a new value automatically increments the
    /// version of the entire NodeState regardless of whether the
    /// value is really changed or not.
    pub fn set<K: ToString, V: ToString>(&mut self, key: K, value: V) {
        let new_version = self.max_version + 1;
        self.set_with_version(key.to_string(), value.to_string(), new_version);
    }

    fn set_with_version(&mut self, key: String, value: String, version: Version) {
        assert!(version > self.max_version);
        let value_size = value.bytes().len();
        assert!(
            value_size <= MAX_KV_VALUE_SIZE,
            "Value for key `{}` is too large (actual: {}, maximum: {})",
            key,
            value_size,
            MAX_KV_VALUE_SIZE
        );
        self.max_version = version;
        self.key_values
            .insert(key, VersionedValue { version, value });
    }
}
```

## spawn_chitchat

``` rust
/// Launch a new server.
///
/// This will start the Chitchat server as a new Tokio background task.
pub async fn spawn_chitchat(
    config: ChitchatConfig,
    initial_key_values: Vec<(String, String)>,
    transport: &dyn Transport,
) -> anyhow::Result<ChitchatHandle> {
    let (command_tx, command_rx) = mpsc::unbounded_channel();

    let seed_addrs: watch::Receiver<HashSet<SocketAddr>> =
        spawn_dns_refresh_loop(&config.seed_nodes).await;

    let socket = transport.open(config.listen_addr).await?;

    let node_id = config.node_id.clone();

    let chitchat = Chitchat::with_node_id_and_seeds(config, seed_addrs, initial_key_values);
    let chitchat_arc = Arc::new(Mutex::new(chitchat));
    let chitchat_arc_clone = chitchat_arc.clone();

    let join_handle = tokio::spawn(async move {
        Server::new(command_rx, chitchat_arc_clone, socket)
            .await
            .run()
            .await
    });

    Ok(ChitchatHandle {
        node_id,
        command_tx,
        chitchat: chitchat_arc,
        join_handle,
    })
}

```

## Server

``` rust
/// UDP server for Chitchat communication.
struct Server {
    command_rx: UnboundedReceiver<Command>,
    chitchat: Arc<Mutex<Chitchat>>,
    transport: Box<dyn Socket>,
    rng: SmallRng,
}

impl Server {
    async fn new(
        command_rx: UnboundedReceiver<Command>,
        chitchat: Arc<Mutex<Chitchat>>,
        transport: Box<dyn Socket>,
    ) -> Self {
        let rng = SmallRng::from_rng(thread_rng()).expect("Failed to seed random generator");
        Self {
            chitchat,
            command_rx,
            transport,
            rng,
        }
    }

    /// Listen for new Chitchat messages.
    async fn run(&mut self) -> anyhow::Result<()> {
        let gossip_interval = self.chitchat.lock().await.config.gossip_interval;
        let mut gossip_interval = time::interval(gossip_interval);
        loop {
            tokio::select! {
                result = self.transport.recv() => match result {
                    Ok((from_addr, message)) => {
                        let _ = self.handle_message(from_addr, message).await;
                    }
                    Err(err) => return Err(err),
                },
                _ = gossip_interval.tick() => {
                    self.gossip_multiple().await
                },
                command = self.command_rx.recv() => match command {
                    Some(Command::Gossip(addr)) => {
                        let _ = self.gossip(addr).await;
                    },
                    Some(Command::Shutdown) | None => break,
                }
            }
        }
        Ok(())
    }

    /// Process a single UDP packet.
    async fn handle_message(
        &mut self,
        from_addr: SocketAddr,
        message: ChitchatMessage,
    ) -> anyhow::Result<()> {
        // Handle gossip from other servers.
        let response = self.chitchat.lock().await.process_message(message);
        // Send reply if necessary.
        if let Some(message) = response {
            self.transport.send(from_addr, message).await?;
        }
        Ok(())
    }

    /// Gossip to multiple randomly chosen nodes.
    async fn gossip_multiple(&mut self) {
        // Gossip with live nodes & probabilistically include a random dead node
        let mut chitchat_guard = self.chitchat.lock().await;
        let cluster_state = chitchat_guard.cluster_state();

        let peer_nodes = cluster_state
            .nodes()
            .filter(|node_id| *node_id != chitchat_guard.self_node_id())
            .map(|node_id| node_id.gossip_public_address)
            .collect::<HashSet<_>>();
        let live_nodes = chitchat_guard
            .live_nodes()
            .map(|node_id| node_id.gossip_public_address)
            .collect::<HashSet<_>>();
        let dead_nodes = chitchat_guard
            .dead_nodes()
            .map(|node_id| node_id.gossip_public_address)
            .collect::<HashSet<_>>();
        let seed_nodes: HashSet<SocketAddr> = chitchat_guard.seed_nodes();
        let (selected_nodes, random_dead_node_opt, random_seed_node_opt) = select_nodes_for_gossip(
            &mut self.rng,
            peer_nodes,
            live_nodes,
            dead_nodes,
            seed_nodes,
        );

        chitchat_guard.update_heartbeat();

        // Drop lock to prevent deadlock in [`UdpSocket::gossip`].
        drop(chitchat_guard);

        for node in selected_nodes {
            let result = self.gossip(node).await;
            if result.is_err() {
                error!(node = ?node, "Gossip error with a live node.");
            }
        }

        if let Some(random_dead_node) = random_dead_node_opt {
            let result = self.gossip(random_dead_node).await;
            if result.is_err() {
                error!(node = ?random_dead_node, "Gossip error with a dead node.")
            }
        }

        if let Some(random_seed_node) = random_seed_node_opt {
            let result = self.gossip(random_seed_node).await;
            if result.is_err() {
                error!(node = ?random_seed_node, "Gossip error with a seed node.")
            }
        }

        // Update nodes liveliness
        let mut chitchat_guard = self.chitchat.lock().await;
        chitchat_guard.update_nodes_liveliness();
    }

    /// Gossip to one other UDP server.
    async fn gossip(&mut self, addr: SocketAddr) -> anyhow::Result<()> {
        let syn = self.chitchat.lock().await.create_syn_message();
        self.transport.send(addr, syn).await?;
        Ok(())
    }
}
```

## ChitchatHandle

``` rust
/// UDP Chitchat server handler.
///
/// It is necessary to hold (and not drop) the handler
/// for the server to keep running.
pub struct ChitchatHandle {
    node_id: NodeId,
    command_tx: UnboundedSender<Command>,
    chitchat: Arc<Mutex<Chitchat>>,
    join_handle: JoinHandle<Result<(), anyhow::Error>>,
}

impl ChitchatHandle {
    pub fn node_id(&self) -> &NodeId {
        &self.node_id
    }

    pub fn chitchat(&self) -> Arc<Mutex<Chitchat>> {
        self.chitchat.clone()
    }

    /// Call a function with mutable access to the [`Chitchat`].
    pub async fn with_chitchat<F, T>(&self, mut fun: F) -> T
    where F: FnMut(&mut Chitchat) -> T {
        let mut chitchat = self.chitchat.lock().await;
        fun(&mut chitchat)
    }

    /// Shut the server down.
    pub async fn shutdown(self) -> Result<(), anyhow::Error> {
        let _ = self.command_tx.send(Command::Shutdown);
        self.join_handle.await?
    }

    /// Perform a Chitchat "handshake" with another UDP server.
    pub fn gossip(&self, addr: SocketAddr) -> Result<(), anyhow::Error> {
        self.command_tx.send(Command::Gossip(addr))?;
        Ok(())
    }
}
```
