# rust raft note

[raft-rs](https://github.com/tikv/raft-rs)
[Floating the Sawtooth Raft: Implementing a Consensus Algorithm in Rust](https://www.hyperledger.org/blog/2019/01/11/floating-the-sawtooth-raft-implementing-a-consensus-algorithm-in-rust)
[sawtooth-raft](https://github.com/hyperledger/sawtooth-raft)
[async-raft](https://github.com/async-raft/async-raft)
[将 paxos 和 raft 统一到一个协议下: abstract-paxos](https://blog.openacid.com/algo/abstract-paxos/)
[raft-zh_cn](https://github.com/maemual/raft-zh_cn)
[OpenRaft 在交易撮合引擎中的应用](https://www.cnblogs.com/databend/p/16544634.html)


## openraft import trait

AppData
``` rust
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ExampleRequest {/* fields */}
impl AppData for ExampleRequest {}
```

AppDataResponse

``` rust
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ExampleResponse(Result<Option<String>, ClientError>);
impl AppDataResponse for ExampleResponse {}
```

RaftStorage

``` rust
pub trait RaftStorage<D, R>: Send + Sync + 'static
where
    D: AppData,
    R: AppDataResponse,
{
    type SnapshotData: AsyncRead + AsyncWrite + AsyncSeek + Send + Sync + Unpin + 'static;

// ...
}
```

RaftNetwork

``` rust
pub trait RaftNetwork<D>: Send + Sync + 'static
where
    D: AppData,
{
    fn send_append_entries<'life0, 'async_trait>(
        &'life0 self,
        target: NodeId,
        rpc: AppendEntriesRequest<D>
    ) -> Pin<Box<dyn Future<Output = Result<AppendEntriesResponse>> + Send + 'async_trait>>
    where
        'life0: 'async_trait,
        Self: 'async_trait;
    fn send_install_snapshot<'life0, 'async_trait>(
        &'life0 self,
        target: NodeId,
        rpc: InstallSnapshotRequest
    ) -> Pin<Box<dyn Future<Output = Result<InstallSnapshotResponse>> + Send + 'async_trait>>
    where
        'life0: 'async_trait,
        Self: 'async_trait;
    fn send_vote<'life0, 'async_trait>(
        &'life0 self,
        target: NodeId,
        rpc: VoteRequest
    ) -> Pin<Box<dyn Future<Output = Result<VoteResponse>> + Send + 'async_trait>>
    where
        'life0: 'async_trait,
        Self: 'async_trait;
}
```

copy from [Getting Started](https://datafuselabs.github.io/openraft/getting-started.html)

## Raft

``` rust
pub fn new(
    id: NodeId,
    config: Arc<Config>,
    network: Arc<N>,
    storage: Arc<S>
) -> Self

pub async fn vote(&self, rpc: VoteRequest) -> Result<VoteResponse, RaftError>

pub async fn current_leader(&self) -> Option<NodeId>

```
copy from [Struct openraft::raft::Raft](https://docs.rs/openraft/latest/openraft/raft/struct.Raft.html)
