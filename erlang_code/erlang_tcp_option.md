# erlang gen_tcp option
## option documentation
copy from erlang documentation inet module doc

```
setopts(Socket, Options) -> ok | {error, posix()}

Types:

Socket = socket()
Options = [socket_setopt()]
Sets one or more options for a socket.

The following options are available:

{active, true | false | once | N}
If the value is true, which is the default, everything received from the socket is sent as messages to the receiving process.

If the value is false (passive mode), the process must explicitly receive incoming data by calling gen_tcp:recv/2,3, gen_udp:recv/2,3, or gen_sctp:recv/1,2 (depending on the type of socket).

If the value is once ({active, once}), one data message from the socket is sent to the process. To receive one more message, setopts/2 must be called again with option {active, once}.

If the value is an integer N in the range -32768 to 32767 (inclusive), the value is added to the socket's count of data messages sent to the controlling process. A socket's default message count is 0. If a negative value is specified, and its magnitude is equal to or greater than the socket's current message count, the socket's message count is set to 0. Once the socket's message count reaches 0, either because of sending received data messages to the process or by being explicitly set, the process is then notified by a special message, specific to the type of socket, that the socket has entered passive mode. Once the socket enters passive mode, to receive more messages setopts/2 must be called again to set the socket back into an active mode.

When using {active, once} or {active, N}, the socket changes behavior automatically when data is received. This can be confusing in combination with connection-oriented sockets (that is, gen_tcp), as a socket with {active, false} behavior reports closing differently than a socket with {active, true} behavior. To simplify programming, a socket where the peer closed, and this is detected while in {active, false} mode, still generates message {tcp_closed,Socket} when set to {active, once}, {active, true}, or {active, N} mode. It is therefore safe to assume that message {tcp_closed,Socket}, possibly followed by socket port termination (depending on option exit_on_close) eventually appears when a socket changes back and forth between {active, true} and {active, false} mode. However, when peer closing is detected it is all up to the underlying TCP/IP stack and protocol.

Notice that {active, true} mode provides no flow control; a fast sender can easily overflow the receiver with incoming messages. The same is true for {active, N} mode, while the message count is greater than zero.

Use active mode only if your high-level protocol provides its own flow control (for example, acknowledging received messages) or the amount of data exchanged is small. {active, false} mode, use of the {active, once} mode, or {active, N} mode with values of N appropriate for the application provides flow control. The other side cannot send faster than the receiver can read.

{broadcast, Boolean} (UDP sockets)
Enables/disables permission to send broadcasts.

{buffer, Size}
The size of the user-level software buffer used by the driver. Not to be confused with options sndbuf and recbuf, which correspond to the Kernel socket buffers. It is recommended to have val(buffer) >= max(val(sndbuf),val(recbuf)) to avoid performance issues because of unnecessary copying. val(buffer) is automatically set to the above maximum when values sndbuf or recbuf are set. However, as the sizes set for sndbuf and recbuf usually become larger, you are encouraged to use getopts/2 to analyze the behavior of your operating system.

{delay_send, Boolean}
Normally, when an Erlang process sends to a socket, the driver tries to send the data immediately. If that fails, the driver uses any means available to queue up the message to be sent whenever the operating system says it can handle it. Setting {delay_send, true} makes all messages queue up. The messages sent to the network are then larger but fewer. The option affects the scheduling of send requests versus Erlang processes instead of changing any real property of the socket. The option is implementation-specific. Defaults to false.

{deliver, port | term}
When {active, true}, data is delivered on the form port : {S, {data, [H1,..Hsz | Data]}} or term : {tcp, S, [H1..Hsz | Data]}.

{dontroute, Boolean}
Enables/disables routing bypass for outgoing messages.

{exit_on_close, Boolean}
This option is set to true by default.

The only reason to set it to false is if you want to continue sending data to the socket after a close is detected, for example, if the peer uses gen_tcp:shutdown/2 to shut down the write side.

{header, Size}
This option is only meaningful if option binary was specified when the socket was created. If option header is specified, the first Size number bytes of data received from the socket are elements of a list, and the remaining data is a binary specified as the tail of the same list. For example, if Size == 2, the data received matches [Byte1,Byte2|Binary].

{high_msgq_watermark, Size}
The socket message queue is set to a busy state when the amount of data on the message queue reaches this limit. Notice that this limit only concerns data that has not yet reached the ERTS internal socket implementation. Defaults to 8 kB.

Senders of data to the socket are suspended if either the socket message queue is busy or the socket itself is busy.

For more information, see options low_msgq_watermark, high_watermark, and low_watermark.

Notice that distribution sockets disable the use of high_msgq_watermark and low_msgq_watermark. Instead use the distribution buffer busy limit, which is a similar feature.

{high_watermark, Size} (TCP/IP sockets)
The socket is set to a busy state when the amount of data queued internally by the ERTS socket implementation reaches this limit. Defaults to 8 kB.

Senders of data to the socket are suspended if either the socket message queue is busy or the socket itself is busy.

For more information, see options low_watermark, high_msgq_watermark, and low_msqg_watermark.

{ipv6_v6only, Boolean}
Restricts the socket to use only IPv6, prohibiting any IPv4 connections. This is only applicable for IPv6 sockets (option inet6).

On most platforms this option must be set on the socket before associating it to an address. It is therefore only reasonable to specify it when creating the socket and not to use it when calling function (setopts/2) containing this description.

The behavior of a socket with this option set to true is the only portable one. The original idea when IPv6 was new of using IPv6 for all traffic is now not recommended by FreeBSD (you can use {ipv6_v6only,false} to override the recommended system default value), forbidden by OpenBSD (the supported GENERIC kernel), and impossible on Windows (which has separate IPv4 and IPv6 protocol stacks). Most Linux distros still have a system default value of false. This policy shift among operating systems to separate IPv6 from IPv4 traffic has evolved, as it gradually proved hard and complicated to get a dual stack implementation correct and secure.

On some platforms, the only allowed value for this option is true, for example, OpenBSD and Windows. Trying to set this option to false, when creating the socket, fails in this case.

Setting this option on platforms where it does not exist is ignored. Getting this option with getopts/2 returns no value, that is, the returned list does not contain an {ipv6_v6only,_} tuple. On Windows, the option does not exist, but it is emulated as a read-only option with value true.

Therefore, setting this option to true when creating a socket never fails, except possibly on a platform where you have customized the kernel to only allow false, which can be doable (but awkward) on, for example, OpenBSD.

If you read back the option value using getopts/2 and get no value, the option does not exist in the host operating system. The behavior of both an IPv6 and an IPv4 socket listening on the same port, and for an IPv6 socket getting IPv4 traffic is then no longer predictable.

{keepalive, Boolean}(TCP/IP sockets)
Enables/disables periodic transmission on a connected socket when no other data is exchanged. If the other end does not respond, the connection is considered broken and an error message is sent to the controlling process. Defaults to disabled.

{linger, {true|false, Seconds}}
Determines the time-out, in seconds, for flushing unsent data in the close/1 socket call. If the first component of the value tuple is false, the second is ignored. This means that close/1 returns immediately, not waiting for data to be flushed. Otherwise, the second component is the flushing time-out, in seconds.

{low_msgq_watermark, Size}
If the socket message queue is in a busy state, the socket message queue is set in a not busy state when the amount of data queued in the message queue falls below this limit. Notice that this limit only concerns data that has not yet reached the ERTS internal socket implementation. Defaults to 4 kB.

Senders that are suspended because of either a busy message queue or a busy socket are resumed when the socket message queue and the socket are not busy.

For more information, see options high_msgq_watermark, high_watermark, and low_watermark.

Notice that distribution sockets disable the use of high_msgq_watermark and low_msgq_watermark. Instead they use the distribution buffer busy limit, which is a similar feature.

{low_watermark, Size} (TCP/IP sockets)
If the socket is in a busy state, the socket is set in a not busy state when the amount of data queued internally by the ERTS socket implementation falls below this limit. Defaults to 4 kB.

Senders that are suspended because of a busy message queue or a busy socket are resumed when the socket message queue and the socket are not busy.

For more information, see options high_watermark, high_msgq_watermark, and low_msgq_watermark.

{mode, Mode :: binary | list}
Received Packet is delivered as defined by Mode.

{netns, Namespace :: file:filename_all()}
Sets a network namespace for the socket. Parameter Namespace is a filename defining the namespace, for example, "/var/run/netns/example", typically created by command ip netns add example. This option must be used in a function call that creates a socket, that is, gen_tcp:connect/3,4, gen_tcp:listen/2, gen_udp:open/1,2, or gen_sctp:open/0,1,2.

This option uses the Linux-specific syscall setns(), such as in Linux kernel 3.0 or later, and therefore only exists when the runtime system is compiled for such an operating system.

The virtual machine also needs elevated privileges, either running as superuser or (for Linux) having capability CAP_SYS_ADMIN according to the documentation for setns(2). However, during testing also CAP_SYS_PTRACE and CAP_DAC_READ_SEARCH have proven to be necessary.

Example:

setcap cap_sys_admin,cap_sys_ptrace,cap_dac_read_search+epi beam.smp
Notice that the filesystem containing the virtual machine executable (beam.smp in the example) must be local, mounted without flag nosetuid, support extended attributes, and the kernel must support file capabilities. All this runs out of the box on at least Ubuntu 12.04 LTS, except that SCTP sockets appear to not support network namespaces.

Namespace is a filename and is encoded and decoded as discussed in module file, with the following exceptions:

Emulator flag +fnu is ignored.

getopts/2 for this option returns a binary for the filename if the stored filename cannot be decoded. This is only to occur if you set the option using a binary that cannot be decoded with the emulator's filename encoding: file:native_name_encoding/0.

list
Received Packet is delivered as a list.

binary
Received Packet is delivered as a binary.

{nodelay, Boolean}(TCP/IP sockets)
If Boolean == true, option TCP_NODELAY is turned on for the socket, which means that also small amounts of data are sent immediately.

{packet, PacketType}(TCP/IP sockets)
Defines the type of packets to use for a socket. Possible values:

raw | 0
No packaging is done.

1 | 2 | 4
Packets consist of a header specifying the number of bytes in the packet, followed by that number of bytes. The header length can be one, two, or four bytes, and containing an unsigned integer in big-endian byte order. Each send operation generates the header, and the header is stripped off on each receive operation.

The 4-byte header is limited to 2Gb.

asn1 | cdr | sunrm | fcgi | tpkt | line
These packet types only have effect on receiving. When sending a packet, it is the responsibility of the application to supply a correct header. On receiving, however, one message is sent to the controlling process for each complete packet received, and, similarly, each call to gen_tcp:recv/2,3 returns one complete packet. The header is not stripped off.

The meanings of the packet types are as follows:

asn1 - ASN.1 BER
sunrm - Sun's RPC encoding
cdr - CORBA (GIOP 1.1)
fcgi - Fast CGI
tpkt - TPKT format [RFC1006]
line - Line mode, a packet is a line-terminated with newline, lines longer than the receive buffer are truncated
http | http_bin
The Hypertext Transfer Protocol. The packets are returned with the format according to HttpPacket described in erlang:decode_packet/3 in ERTS. A socket in passive mode returns {ok, HttpPacket} from gen_tcp:recv while an active socket sends messages like {http, Socket, HttpPacket}.

httph | httph_bin
These two types are often not needed, as the socket automatically switches from http/http_bin to httph/httph_bin internally after the first line is read. However, there can be occasions when they are useful, such as parsing trailers from chunked encoding.

{packet_size, Integer}(TCP/IP sockets)
Sets the maximum allowed length of the packet body. If the packet header indicates that the length of the packet is longer than the maximum allowed length, the packet is considered invalid. The same occurs if the packet header is too large for the socket receive buffer.

For line-oriented protocols (line, http*), option packet_size also guarantees that lines up to the indicated length are accepted and not considered invalid because of internal buffer limitations.

{line_delimiter, Char}(TCP/IP sockets)
Sets the line delimiting character for line-oriented protocols (line). Defaults to $\n.

{raw, Protocol, OptionNum, ValueBin}
See below.

{read_packets, Integer}(UDP sockets)
Sets the maximum number of UDP packets to read without intervention from the socket when data is available. When this many packets have been read and delivered to the destination process, new packets are not read until a new notification of available data has arrived. Defaults to 5. If this parameter is set too high, the system can become unresponsive because of UDP packet flooding.

{recbuf, Size}
The minimum size of the receive buffer to use for the socket. You are encouraged to use getopts/2 to retrieve the size set by your operating system.

{reuseaddr, Boolean}
Allows or disallows local reuse of port numbers. By default, reuse is disallowed.

{send_timeout, Integer}
Only allowed for connection-oriented sockets.

Specifies a longest time to wait for a send operation to be accepted by the underlying TCP stack. When the limit is exceeded, the send operation returns {error,timeout}. How much of a packet that got sent is unknown; the socket is therefore to be closed whenever a time-out has occurred (see send_timeout_close below). Defaults to infinity.

{send_timeout_close, Boolean}
Only allowed for connection-oriented sockets.

Used together with send_timeout to specify whether the socket is to be automatically closed when the send operation returns {error,timeout}. The recommended setting is true, which automatically closes the socket. Defaults to false because of backward compatibility.

{show_econnreset, Boolean}(TCP/IP sockets)
When this option is set to false, which is default, an RST received from the TCP peer is treated as a normal close (as though an FIN was sent). A caller to gen_tcp:recv/2 gets {error, closed}. In active mode, the controlling process receives a {tcp_close, Socket} message, indicating that the peer has closed the connection.

Setting this option to true allows you to distinguish between a connection that was closed normally, and one that was aborted (intentionally or unintentionally) by the TCP peer. A call to gen_tcp:recv/2 returns {error, econnreset}. In active mode, the controlling process receives a {tcp_error, Socket, econnreset} message before the usual {tcp_closed, Socket}, as is the case for any other socket error. Calls to gen_tcp:send/2 also returns {error, econnreset} when it is detected that a TCP peer has sent an RST.

A connected socket returned from gen_tcp:accept/1 inherits the show_econnreset setting from the listening socket.

{sndbuf, Size}
The minimum size of the send buffer to use for the socket. You are encouraged to use getopts/2, to retrieve the size set by your operating system.

{priority, Integer}
Sets the SO_PRIORITY socket level option on platforms where this is implemented. The behavior and allowed range varies between different systems. The option is ignored on platforms where it is not implemented. Use with caution.

{tos, Integer}
Sets IP_TOS IP level options on platforms where this is implemented. The behavior and allowed range varies between different systems. The option is ignored on platforms where it is not implemented. Use with caution.

{tclass, Integer}
Sets IPV6_TCLASS IP level options on platforms where this is implemented. The behavior and allowed range varies between different systems. The option is ignored on platforms where it is not implemented. Use with caution.

In addition to these options, raw option specifications can be used. The raw options are specified as a tuple of arity four, beginning with tag raw, followed by the protocol level, the option number, and the option value specified as a binary. This corresponds to the second, third, and fourth arguments to the setsockopt call in the C socket API. The option value must be coded in the native endianess of the platform and, if a structure is required, must follow the structure alignment conventions on the specific platform.

Using raw socket options requires detailed knowledge about the current operating system and TCP stack.
```

## random port
copy from lasp_sup.erl

```
%% @private
random_port() ->
    {ok, Socket} = gen_tcp:listen(0, []),
    {ok, {_, Port}} = inet:sockname(Socket),
    ok = gen_tcp:close(Socket),
    Port.
```
The documentation is:

```
gen_tcp:listen(Port, Options)
If Port == 0, the underlying OS assigns an available port number, use inet:port/1 to retrieve it.

```

## prim_net:async_accept/2
```erlang
listen_on(CallbackModule, IpAddr, Port) when is_tuple(IpAddr) andalso
                                             (8 =:= size(IpAddr) orelse
                                              4 =:= size(IpAddr)) ->
    SockOpts = [{ip, IpAddr}|CallbackModule:sock_opts()],
    case gen_tcp:listen(Port, SockOpts) of
        {ok, LSock} ->
            {ok, _Ref} = prim_inet:async_accept(LSock, -1),
            {ok, LSock};
        Err ->
            Err
    end;
```
This function is `undocumented`, and use [ranch](https://github.com/ninenines/ranch) as much as possible.

## shutdown socket
Closes a socket in one or two directions, with option {exit_on_close, false}. Not close socket.
``` erlang
shutdown(Socket, How) -> ok | {error, Reason}
Types

Socket = socket()
How = read | write | read_write
Reason = inet:posix()
Closes a socket in one or two directions.

How == write means closing the socket for writing, reading from it is still possible.

If How == read or there is no outgoing data buffered in the Socket port, the socket is shut down immediately and any error encountered is returned in Reason.

If there is data buffered in the socket port, the attempt to shutdown the socket is postponed until that data is written to the kernel socket send buffer. If any errors are encountered, the socket is closed and {error, closed} is returned on the next recv/2 or send/2.

Option {exit_on_close, false} is useful if the peer has done a shutdown on the write side.
```

## controlling_process

``` erlang
%% copy from gen_tcp.erl
controlling_process(S, NewOwner) ->
    case inet_db:lookup_socket(S) of
	{ok, _Mod} -> % Just check that this is an open socket
	    inet:tcp_controlling_process(S, NewOwner);
	Error ->
	    Error
    end.
```
The `gen_tcp:controlling_process/2` is almost the same `inet:tcp_controlling_process/2`

## Erlang: Avoiding race condition with gen_tcp:controlling_process
see [Erlang: Avoiding race condition with gen_tcp:controlling_process](https://stackoverflow.com/questions/11409656/erlang-avoiding-race-condition-with-gen-tcpcontrolling-process)
``` erlang
-define(TCP_OPTIONS, [binary, {active, false}, ...]).

...

start(Port) ->
    {ok, Socket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
    accept(Socket).

accept(ListenSocket) ->
    case gen_tcp:accept(ListenSocket) of
        {ok, Socket} ->
            Pid = spawn(fun() ->
                io:format("Connection accepted ~n", []),
                enter_loop(Socket)
            end),
            gen_tcp:controlling_process(Socket, Pid),
            Pid ! ack,
            accept(ListenSocket);
        Error ->
            exit(Error)
    end.

enter_loop(Sock) ->
    %% make sure to acknowledge owner rights transmission finished
    receive ack -> ok end,
    loop(Sock).

loop(Sock) ->
    %% set soscket options to receive messages directly into itself
    inet:setopts(Sock, [{active, once}]),
    receive
        {tcp, Socket, Data} ->
            io:format("Got packet: ~p~n", [Data]),
            ...,
            loop(Socket);
        {tcp_closed, Socket} ->
            io:format("Socket ~p closed~n", [Socket]);
        {tcp_error, Socket, Reason} ->
            io:format("Error on socket ~p reason: ~p~n", [Socket, Reason])
    end.
```
the esockd and ranch project use the same method like above to avoid race condition.
