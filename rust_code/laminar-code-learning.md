# laminar code learning

## udp packet types

src/packet/packet_structure.rs
``` rust
Packet::unreliable(addr: SocketAddr, payload: Vec<u8>)
Packet::unreliable_sequenced(addr: SocketAddr, payload: Vec<u8>, stream_id: Option<u8>)
Packet::reliable_unordered(addr: SocketAddr, payload: Vec<u8>)
Packet::reliable_ordered(addr: SocketAddr, payload: Vec<u8>, stream_id: Option<u8>)
Packet::reliable_sequenced(addr: SocketAddr, payload: Vec<u8>, stream_id: Option<u8>)
```

## udp header info

AckedPacketHeader definition is in src/packet/header/acked_packet_header.rs
AckedPacketHeader has 8 bytes
``` rust
#[derive(Copy, Clone, Debug)]
/// This header provides reliability information.
pub struct AckedPacketHeader {
    /// This is the sequence number so that we can know where in the sequence of packages this packet belongs.
    pub seq: u16,
    // This is the last acknowledged sequence number.
    ack_seq: u16,
    // This is an bitfield of all last 32 acknowledged packages
    ack_field: u32,
}

impl HeaderWriter for AckedPacketHeader {
    type Output = Result<()>;

    fn parse(&self, buffer: &mut Vec<u8>) -> Self::Output {
        buffer.write_u16::<BigEndian>(self.seq)?;
        buffer.write_u16::<BigEndian>(self.ack_seq)?;
        buffer.write_u32::<BigEndian>(self.ack_field)?;
        Ok(())
    }
}
```

ArrangingHeader definition is in src/packet/header/arranging_header.rs

ArrangingHeader has 3 bytes
``` rust
#[derive(Copy, Clone, Debug)]
/// This header represents a fragmented packet header.
pub struct ArrangingHeader {
    arranging_id: SequenceNumber,
    stream_id: u8,
}

impl HeaderWriter for ArrangingHeader {
    type Output = Result<()>;

    fn parse(&self, buffer: &mut Vec<u8>) -> Self::Output {
        buffer.write_u16::<BigEndian>(self.arranging_id)?;
        buffer.write_u8(self.stream_id)?;

        Ok(())
    }
}
```
FragmentHeader definition is in src/packet/header/fragment_header.rs
FragmentHeader has 4 bytes

``` rust
#[derive(Copy, Clone, Debug)]
/// This header represents a fragmented packet header.
pub struct FragmentHeader {
    sequence: u16,
    id: u8,
    num_fragments: u8,
}

impl HeaderWriter for FragmentHeader {
    type Output = Result<()>;

    fn parse(&self, buffer: &mut Vec<u8>) -> Self::Output {
        buffer.write_u16::<BigEndian>(self.sequence)?;
        buffer.write_u8(self.id)?;
        buffer.write_u8(self.num_fragments)?;

        Ok(())
    }
}
```
StandardHeader definition is in src/packet/header/standard_header.rs
StandardHeader has 5 bytes
``` rust
#[derive(Copy, Clone, Debug)]
/// This header will be included in each packet, and contains some basic information.
pub struct StandardHeader {
    protocol_version: u16,
    packet_type: PacketType,
    delivery_guarantee: DeliveryGuarantee,
    ordering_guarantee: OrderingGuarantee,
}

impl HeaderWriter for StandardHeader {
    type Output = Result<()>;

    fn parse(&self, buffer: &mut Vec<u8>) -> Self::Output {
        buffer.write_u16::<BigEndian>(self.protocol_version)?;
        buffer.write_u8(self.packet_type.to_u8())?;
        buffer.write_u8(self.delivery_guarantee.to_u8())?;
        buffer.write_u8(self.ordering_guarantee.to_u8())?;
        Ok(())
    }
}

```

## sending udp code learning

src/net/socket.rs functions will call self.handler.manual_poll(time);
and `handler` is just ConnectionManager<SocketWithConditioner, VirtualConnection>,
``` rust
Socket::start_polling(&mut self)
Socket::start_polling_with_duration(&mut self, sleep_duration: Option<Duration>)
Socket::manual_poll(&mut self, time: Instant)
```

src/net/connection_Manager.rs call manual_poll(&mut self, time: Instant)
manual_poll() method will call conn.process_packet(messenger, payload, time);

``` rust
let mut conn = TConnection::create_connection(messenger, address, time);
conn.process_packet(messenger, payload, time);
```
process_packet method will handle the received udp data.

manual_poll() method will call the code below:

``` rust
// now grab all the waiting packets and send them
while let Ok(event) = self.user_event_receiver.try_recv() {
    // get or create connection
    let conn = self.connections.entry(event.address()).or_insert_with(|| {
    TConnection::create_connection(messenger, event.address(), time)
    });

    let was_est = conn.is_established();
    conn.process_event(messenger, event, time);
    if !was_est && conn.is_established() {
    unestablished_connections -= 1;
    }
}
```
try_recv() method will receive the udp data packet that user want to send，and then call process_event() method to make binary data and send it to the dest address.

src/net/connection_impl.rs module implements VirtualConnection Connection trait
``` rust
VirtualConnection::process_event(
        &mut self,
        messenger: &mut impl ConnectionMessenger<Self::ReceiveEvent>,
        event: Self::SendEvent,
        time: Instant,
    )
```

process_event() method call below code:

``` rust
send_packets(
    messenger,
    &addr,
    self.process_outgoing(
        PacketInfo::user_packet(
            event.payload(),
            event.delivery_guarantee(),
            event.order_guarantee(),
        ),
        None,
        time,
        ),
    "user packet",
    );
```

The process_outgoing() method makes Packet data to OutgoingPackets by the suitable information.
In src/net/virtual_connection.rs module:
``` rust
VirtualConnection::process_outgoing<'a>(&mut self, packet: PacketInfo<'a>, last_item_identifier: Option<SequenceNumber>, time: Instant)
```

In src/net/connection_impl.rs module, it defines send_packet() function.

``` rust
// Sends multiple outgoing packets.
fn send_packets(
    ctx: &mut impl ConnectionMessenger<SocketEvent>,
    address: &SocketAddr,
    packets: Result<OutgoingPackets>,
    err_context: &str,
) {
    match packets {
        Ok(packets) => {
            for outgoing in packets {
                ctx.send_packet(address, &outgoing.contents());
            }
        }
        Err(error) => error!("Error occured processing {}: {:?}", err_context, error),
    }
}
```
And OutgoingPackets definition is:

``` rust
/// Packet that that contains data which is ready to be sent to a remote endpoint.
#[derive(Debug)]
pub struct OutgoingPacket<'p> {
    header: Vec<u8>,
    payload: &'p [u8],
}

impl<'p> OutgoingPacket<'p> {
    /// Return the contents of this packet; the content includes the header and payload bytes.
    ///
    /// # Remark
    /// - Until here we could use a reference to the outgoing data but here we need to do a hard copy.
    /// Because the header could vary in size but should be in front of the payload provided by the user.
    pub fn contents(&self) -> Box<[u8]> {
        [self.header.as_slice(), &self.payload]
            .concat()
            .into_boxed_slice()
    }
}

```
And send_packet() method is defined in src/net/socket.rs.

``` rust
#[derive(Debug)]
struct SocketWithConditioner {
    is_blocking_mode: bool,
    socket: UdpSocket,
    link_conditioner: Option<LinkConditioner>,
}

mpl DatagramSocket for SocketWithConditioner {
    // Determinate whether packet will be sent or not based on `LinkConditioner` if enabled.
    fn send_packet(&mut self, addr: &SocketAddr, payload: &[u8]) -> std::io::Result<usize> {
        if cfg!(feature = "tester") {
            if let Some(ref mut link) = &mut self.link_conditioner {
                if !link.should_send() {
                    return Ok(0);
                }
            }
        }
        self.socket.send_to(payload, addr)
    }
}
In fact, it calls UdpSocket::send_to() method.
```



## receving UDP sequence


In the src/net/connection_Manager.rs module calls manual_poll(&mut self, time: Instant) method.
The method finally calls receive_packet() method.

``` rust
ConnectionManager::manual_poll(&mut self, time: Instant) {
...
let messenger = &mut self.messenger;

        // first we pull all newly arrived packets and handle them
        loop {
            match messenger
                .socket
                .receive_packet(self.receive_buffer.as_mut())
            {
                Ok((payload, address)) => {
                    if let Some(conn) = self.connections.get_mut(&address) {
                        let was_est = conn.is_established();
                        conn.process_packet(messenger, payload, time);
                        if !was_est && conn.is_established() {
                            unestablished_connections -= 1;
                        }
                    } else {
                        let mut conn = TConnection::create_connection(messenger, address, time);
                        conn.process_packet(messenger, payload, time);

                        // We only allow a maximum amount number of unestablished connections to bet created
                        // from inbound packets to prevent packet flooding from allocating unbounded memory.
                        if unestablished_connections < self.max_unestablished_connections as usize {
                            self.connections.insert(address, conn);
                            unestablished_connections += 1;
                        }
                    }
                }
                Err(e) => {
                    if e.kind() != std::io::ErrorKind::WouldBlock {
                        error!("Encountered an error receiving data: {:?}", e);
                    }
                    break;
                }
            }
            // prevent from blocking, break after receiving first packet
            if messenger.socket.is_blocking_mode() {
                break;
            }
        }
...
}
```

And receive_packet() is defined in src/net/socket.rs file.

``` rust
#[derive(Debug)]
struct SocketWithConditioner {
    is_blocking_mode: bool,
    socket: UdpSocket,
    link_conditioner: Option<LinkConditioner>,
}

impl SocketWithConditioner {
fn receive_packet<'a>(
        &mut self,
        buffer: &'a mut [u8],
    ) -> std::io::Result<(&'a [u8], SocketAddr)> {
        self.socket
            .recv_from(buffer)
            .map(move |(recv_len, address)| (&buffer[..recv_len], address))
    }
}

```
In fact it call UdpSocket::recv_from() method.

## fragment packet code
In the src/infrastructure/fragmenter.rs

``` rust
impl Fragmentation {
    /// Splits the given payload into fragments and write those fragments to the passed packet data.
    pub fn spit_into_fragments<'a>(payload: &'a [u8], config: &Config) -> Result<Vec<&'a [u8]>> {
        let mut fragments = Vec::new();

        let payload_length = payload.len() as u16;
        let num_fragments =
            // Safe cast max fragments is u8
            Fragmentation::fragments_needed(payload_length, config.fragment_size) as u8;

        if num_fragments > config.max_fragments {
            return Err(FragmentErrorKind::ExceededMaxFragments.into());
        }

        for fragment_id in 0..num_fragments {
            // get start and end position of buffer
            let start_fragment_pos = u16::from(fragment_id) * config.fragment_size;
            let mut end_fragment_pos = (u16::from(fragment_id) + 1) * config.fragment_size;

            // If remaining buffer fits int one packet just set the end position to the length of the packet payload.
            if end_fragment_pos > payload_length {
                end_fragment_pos = payload_length;
            }

            // get specific slice of data for fragment
            let fragment_data = &payload[start_fragment_pos as usize..end_fragment_pos as usize];

            fragments.push(fragment_data);
        }

        Ok(fragments)
    }
}
```

## make the fragment packet into a complete packet

``` rust
impl Fragmentation {

    /// Reads fragment data and return the complete packet when all fragments are received.
    pub fn handle_fragment(
        &mut self,
        fragment_header: FragmentHeader,
        fragment_payload: &[u8],
        acked_header: Option<AckedPacketHeader>,
    ) -> Result<Option<(Vec<u8>, AckedPacketHeader)>> {
        // read fragment packet

        self.create_fragment_if_not_exists(fragment_header);

        let num_fragments_received;
        let num_fragments_total;
        let sequence;
        let total_buffer;

        {
            // get entry of previous received fragments
            let reassembly_data = match self.fragments.get_mut(fragment_header.sequence()) {
                Some(val) => val,
                None => return Err(FragmentErrorKind::CouldNotFindFragmentById.into()),
            };

            // got the data
            if reassembly_data.num_fragments_total != fragment_header.fragment_count() {
                return Err(FragmentErrorKind::FragmentWithUnevenNumberOfFragments.into());
            }

            if usize::from(fragment_header.id()) >= reassembly_data.fragments_received.len() {
                return Err(FragmentErrorKind::ExceededMaxFragments.into());
            }

            if reassembly_data.fragments_received[usize::from(fragment_header.id())] {
                return Err(FragmentErrorKind::AlreadyProcessedFragment.into());
            }

            // increase number of received fragments and set the specific fragment to received
            reassembly_data.num_fragments_received += 1;
            reassembly_data.fragments_received[usize::from(fragment_header.id())] = true;

            // add the payload from the fragment to the buffer whe have in cache
            reassembly_data.buffer.write_all(&*fragment_payload)?;

            if let Some(acked_header) = acked_header {
                if reassembly_data.acked_header.is_none() {
                    reassembly_data.acked_header = Some(acked_header);
                } else {
                    return Err(FragmentErrorKind::MultipleAckHeaders.into());
                }
            }

            num_fragments_received = reassembly_data.num_fragments_received;
            num_fragments_total = reassembly_data.num_fragments_total;
            sequence = reassembly_data.sequence as u16;
            total_buffer = reassembly_data.buffer.clone();
        }

        // if we received all fragments then remove entry and return the total received bytes.
        if num_fragments_received == num_fragments_total {
            let sequence = sequence as u16;
            if let Some(mut reassembly_data) = self.fragments.remove(sequence) {
                if reassembly_data.acked_header.is_none() {
                    return Err(FragmentErrorKind::MissingAckHeader.into());
                }

                let acked_header = reassembly_data.acked_header.take().unwrap();
                return Ok(Some((total_buffer, acked_header)));
            } else {
                return Err(FragmentErrorKind::CouldNotFindFragmentById.into());
            }
        }

        Ok(None)
    }
}
```

## confirm receiving
in the src/net/virtual_connection.rs module:

``` rust
impl VirtualConnection {
/// Processes the incoming data and returns a packet once the data is complete.
    pub fn process_incoming(
        &mut self,
        received_data: &[u8],
        time: Instant,
    ) -> Result<IncomingPackets> {
    ...
    match self.fragmentation.handle_fragment(
                            fragment_header,
                            &payload,
                            acked_header,
                        ) {
                            Ok(Some((payload, acked_header))) => {
                                self.congestion_handler
                                    .process_incoming(acked_header.sequence());
                                self.acknowledge_handler.process_incoming(
                                    acked_header.sequence(),
                                    acked_header.ack_seq(),
                                    acked_header.ack_field(),
                                );
    ...
    }
}

```
It calls src/infrastructure/acknowledge.rs module  process_incoming() method.

``` rust
impl AcknowledgmentHandler {
    /// Process the incoming sequence number.
    ///
    /// - Acknowledge the incoming sequence number
    /// - Update dropped packets
    pub fn process_incoming(
        &mut self,
        remote_seq_num: u16,
        remote_ack_seq: u16,
        mut remote_ack_field: u32,
    ) {
        // ensure that `self.remote_ack_sequence_num` is always increasing (with wrapping)
        if sequence_greater_than(remote_ack_seq, self.remote_ack_sequence_num) {
            self.remote_ack_sequence_num = remote_ack_seq;
        }

        self.received_packets
            .insert(remote_seq_num, ReceivedPacket {});

        // the current `remote_ack_seq` was (clearly) received so we should remove it
        self.sent_packets.remove(&remote_ack_seq);

        // The `remote_ack_field` is going to include whether or not the past 32 packets have been
        // received successfully. If so, we have no need to resend old packets.
        for i in 1..=REDUNDANT_PACKET_ACKS_SIZE {
            let ack_sequence = remote_ack_seq.wrapping_sub(i);
            if remote_ack_field & 1 == 1 {
                self.sent_packets.remove(&ack_sequence);
            }
            remote_ack_field >>= 1;
        }
    }

    /// Returns a `Vec` of packets we believe have been dropped.
    pub fn dropped_packets(&mut self) -> Vec<SentPacket> {
        let mut sent_sequences: Vec<SequenceNumber> = self.sent_packets.keys().cloned().collect();
        sent_sequences.sort_unstable();

        let remote_ack_sequence = self.remote_ack_sequence_num;
        sent_sequences
            .into_iter()
            .filter(|s| {
                if sequence_less_than(*s, remote_ack_sequence) {
                    remote_ack_sequence.wrapping_sub(*s) > REDUNDANT_PACKET_ACKS_SIZE
                } else {
                    false
                }
            })
            .flat_map(|s| self.sent_packets.remove(&s))
            .collect()
    }
}
```
And that extra info is saved while sending the fragment packet.

It is mainly calling src/infrastructure/acknowledge.rs module process_outgoing() method.
``` rust
impl AcknowledgmentHandler {
        /// Enqueues the outgoing packet for acknowledgment.
    pub fn process_outgoing(
        &mut self,
        packet_type: PacketType,
        payload: &[u8],
        ordering_guarantee: OrderingGuarantee,
        item_identifier: Option<SequenceNumber>,
    ) {
        self.sent_packets.insert(
            self.sequence_number,
            SentPacket {
                packet_type,
                payload: Box::from(payload),
                ordering_guarantee,
                item_identifier,
            },
        );

        // bump the local sequence number for the next outgoing packet
        self.sequence_number = self.sequence_number.wrapping_add(1);
    }
}
```
In the src/net/virtual_connection.rs module，it calls the process_outgoing() method.

``` rust
impl VirtualConnection {
    /// Pre-processes the given buffer to be sent over the network.
    pub fn process_outgoing<'a>(
        &mut self,
        packet: PacketInfo<'a>,
        last_item_identifier: Option<SequenceNumber>,
        time: Instant,
    ) -> Result<OutgoingPackets<'a>> {
    ...
    self.acknowledge_handler.process_outgoing(
        packet.packet_type,
        packet.payload,
        packet.ordering,
        item_identifier_value,
    );
    ...
    }
}
```


## resend by timeout
In the src/net/connection_impl.rs module, update() method implement the resend code logic.

``` rust
impl Connection for VirtualConnection {
        /// Processes various connection-related tasks: resend dropped packets, send heartbeat packet, etc...
    /// This function gets called very frequently.
    fn update(
        &mut self,
        messenger: &mut impl ConnectionMessenger<Self::ReceiveEvent>,
        time: Instant,
    ) {
        // resend dropped packets
        for dropped in self.gather_dropped_packets() {
            let packets = self.process_outgoing(
                PacketInfo {
                    packet_type: dropped.packet_type,
                    payload: &dropped.payload,
                    // because a delivery guarantee is only sent with reliable packets
                    delivery: DeliveryGuarantee::Reliable,
                    // this is stored with the dropped packet because they could be mixed
                    ordering: dropped.ordering_guarantee,
                },
                dropped.item_identifier,
                time,
            );
            send_packets(messenger, &self.remote_address, packets, "dropped packets");
        }

        // send heartbeat packets if required
        if self.is_established() {
            if let Some(heartbeat_interval) = messenger.config().heartbeat_interval {
                let addr = self.remote_address;
                if self.last_sent(time) >= heartbeat_interval {
                    send_packets(
                        messenger,
                        &addr,
                        self.process_outgoing(PacketInfo::heartbeat_packet(&[]), None, time),
                        "heatbeat packet",
                    );
                }
            }
        }
    }
}
```
The send_packets() method is analyzed above, it is just send the binary data method.

In the src/net/connection_Manager.rs module，the manual_polling() method calls the method:

``` rust
impl<TSocket: DatagramSocket, TConnection: Connection> ConnectionManager<TSocket, TConnection> {
    /// Processes any inbound/outbound packets and events.
    /// Processes connection specific logic for active connections.
    /// Removes dropped connections from active connections list.
    pub fn manual_poll(&mut self, time: Instant) {
        ...
        // update all connections
        for conn in self.connections.values_mut() {
            conn.update(messenger, time);
        }
        ...
    }
}
```


## avoid udp data traffic
in the src/net/connection_Manager.rs module，manual_polling() method sets limit of the connection number.
``` rust
impl<TSocket: DatagramSocket, TConnection: Connection> ConnectionManager<TSocket, TConnection> {
    /// Processes any inbound/outbound packets and events.
    /// Processes connection specific logic for active connections.
    /// Removes dropped connections from active connections list.
    pub fn manual_poll(&mut self, time: Instant) {
        let mut unestablished_connections = self.unestablished_connection_count();
        ...
        let mut conn = TConnection::create_connection(messenger, address, time);
        conn.process_packet(messenger, payload, time);

        // We only allow a maximum amount number of unestablished connections to bet created
        // from inbound packets to prevent packet flooding from allocating unbounded memory.
        if unestablished_connections < self.max_unestablished_connections as usize {
            self.connections.insert(address, conn);
            unestablished_connections += 1;
        }
    }
}
```
