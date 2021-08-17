# rust network

## listen on all interfaces with TCPListener

``` rust
use std::net::TcpListener;

let listener = TcpListener::bind("[::]:0");

```
`[::]:0` is bind any port in the os, the `[::]:80` is listen on 80 port on all interfaces in the os.
copy from [How to listen on all interfaces with TCPListener? [solved]](https://users.rust-lang.org/t/how-to-listen-on-all-interfaces-with-tcplistener-solved/12269)
