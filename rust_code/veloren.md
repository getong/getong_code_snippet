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

## ecs
veloren uses `specs` to handle the client data.

``` rust
    fn initialize_client(
        &mut self,
        client: crate::connection_handler::IncomingClient,
    ) -> Result<Option<specs::Entity>, Error> {
        if self.settings().max_players <= self.state.ecs().read_storage::<Client>().join().count() {
            trace!(
                ?client.participant,
                "to many players, wont allow participant to connect"
            );
            client.send(ServerInit::TooManyPlayers)?;
            return Ok(None);
        }

        let entity = self
            .state
            .ecs_mut()
            .create_entity_synced()
            .with(client)
            .build();
        self.state
            .ecs()
            .read_resource::<metrics::PlayerMetrics>()
            .clients_connected
            .inc();
        // Send client all the tracked components currently attached to its entity as
        // well as synced resources (currently only `TimeOfDay`)
        debug!("Starting initial sync with client.");
        self.state
            .ecs()
            .read_storage::<Client>()
            .get(entity)
            .expect(
                "We just created this entity with a Client component using build(), and we have \
                 &mut access to the ecs so it can't have been deleted yet.",
            )
            .send(ServerInit::GameSync {
                // Send client their entity
                entity_package: TrackedComps::fetch(self.state.ecs())
                    .create_entity_package(entity, None, None, None)
                    .expect(
                        "We just created this entity as marked() (using create_entity_synced) so \
                         it definitely has a uid",
                    ),
                time_of_day: *self.state.ecs().read_resource(),
                max_group_size: self.settings().max_player_group_size,
                client_timeout: self.settings().client_timeout,
                world_map: self.map.clone(),
                recipe_book: default_recipe_book().cloned(),
                material_stats: MaterialStatManifest::default(),
                ability_map: (&*self
                    .state
                    .ecs()
                    .read_resource::<comp::item::tool::AbilityMap>())
                    .clone(),
            })?;
        Ok(Some(entity))
    }
```
copy from server/src/lib.rs
