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
