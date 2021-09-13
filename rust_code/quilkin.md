# quilkin

## session send packet

``` rust
    /// process_recv_packet processes a packet that is received by this session.
    async fn process_recv_packet(
        log: &Logger,
        metrics: &Metrics,
        sender: &mut mpsc::Sender<Packet>,
        expiration: &Arc<AtomicU64>,
        ttl: Duration,
        packet_ctx: ReceivedPacketContext<'_>,
    ) {
        let ReceivedPacketContext {
            packet,
            filter_manager,
            endpoint,
            from,
            to,
        } = packet_ctx;

        trace!(log, "Received packet"; "from" => from,
            "endpoint_addr" => &endpoint.address,
            "contents" => debug::bytes_to_string(packet));

        if let Err(err) = Session::do_update_expiration(expiration, ttl) {
            warn!(log, "Error updating session expiration"; "error" => %err)
        }

        let filter_chain = {
            let filter_manager_guard = filter_manager.read();
            filter_manager_guard.get_filter_chain()
        };
        if let Some(response) =
            filter_chain.write(WriteContext::new(endpoint, from, to, packet.to_vec()))
        {
            if let Err(err) = sender.send(Packet::new(to, response.contents)).await {
                metrics.rx_errors_total.inc();
                error!(log, "Error sending packet to channel"; "error" => %err);
            }
        } else {
            metrics.packets_dropped_total.inc();
        }
    }
```

copy from session.rs file
