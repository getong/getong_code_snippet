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


## thread_pool

``` rust
let thread_pool = Arc::new(
            ThreadPoolBuilder::new()
                .num_threads(num_cpus::get().max(common::consts::MIN_RECOMMENDED_RAYON_THREADS))
                .thread_name(move |i| format!("rayon-{}-{}", thread_name_infix, i))
                .build()
                .unwrap(),
        );
```

##


## handle new client

``` rust
    async fn init_participant(
        participant: Participant,
        client_sender: Sender<IncomingClient>,
        info_requester_sender: Sender<Sender<ServerInfoPacket>>,
    ) -> Result<(), Box<dyn std::error::Error>> {
        debug!("New Participant connected to the server");
        let (sender, receiver) = bounded(1);
        info_requester_sender.send(sender)?;

        let reliable = Promises::ORDERED | Promises::CONSISTENCY;
        let reliablec = reliable | Promises::COMPRESSED;

        let general_stream = participant.open(3, reliablec, 500).await?;
        let ping_stream = participant.open(2, reliable, 500).await?;
        let mut register_stream = participant.open(3, reliablec, 500).await?;
        let character_screen_stream = participant.open(3, reliablec, 500).await?;
        let in_game_stream = participant.open(3, reliablec, 100_000).await?;
        let terrain_stream = participant.open(4, reliable, 20_000).await?;

        let server_data = receiver.recv()?;

        register_stream.send(server_data.info)?;

        const TIMEOUT: Duration = Duration::from_secs(5);
        let client_type = match select!(
            _ = tokio::time::sleep(TIMEOUT).fuse() => None,
            t = register_stream.recv::<ClientType>().fuse() => Some(t),
        ) {
            None => {
                debug!("Timeout for incoming client elapsed, aborting connection");
                return Ok(());
            },
            Some(client_type) => client_type?,
        };

        let client = Client::new(
            client_type,
            participant,
            server_data.time,
            general_stream,
            ping_stream,
            register_stream,
            character_screen_stream,
            in_game_stream,
            terrain_stream,
        );

        client_sender.send(client)?;
        Ok(())
    }
```
copy from server/src/connection_handler.rs

the recv:

``` rust
    /// Handle new client connections.
    fn handle_new_connections(&mut self, frontend_events: &mut Vec<Event>) {
        while let Ok(sender) = self.connection_handler.info_requester_receiver.try_recv() {
            // can fail, e.g. due to timeout or network prob.
            trace!("sending info to connection_handler");
            let _ = sender.send(crate::connection_handler::ServerInfoPacket {
                info: self.get_server_info(),
                time: self.state.get_time(),
            });
        }

        while let Ok(incoming) = self.connection_handler.client_receiver.try_recv() {
            match self.initialize_client(incoming) {
                Ok(None) => (),
                Ok(Some(entity)) => {
                    frontend_events.push(Event::ClientConnected { entity });
                    debug!("Done initial sync with client.");
                },
                Err(e) => {
                    debug!(?e, "failed initializing a new client");
                },
            }
        }
    }
```

copy from server/src/lib.rs

## common_state::State;
State is just a basic usage of [specs](https://github.com/amethyst/specs)

## common_systems crate is the same basic usage of specs

## veloren use sqlite to store data.
The rusqlite crate is used to handle the connection.


## data migration

``` rust
/// Runs any pending database migrations. This is executed during server startup
pub fn run_migrations(settings: &DatabaseSettings) {
    let mut conn = establish_connection(settings, ConnectionMode::ReadWrite);

    diesel_to_rusqlite::migrate_from_diesel(&mut conn)
        .expect("One-time migration from Diesel to Refinery failed");

    // If migrations fail to run, the server cannot start since the database will
    // not be in the required state.
    let report: Report = embedded::migrations::runner()
        .set_abort_divergent(false)
        .run(&mut conn.connection)
        .expect("Database migrations failed, server startup aborted");

    let applied_migrations = report.applied_migrations().len();
    info!("Applied {} database migrations", applied_migrations);
}
```
