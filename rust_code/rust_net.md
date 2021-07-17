# rust net

## Enum std::net::Shutdown

``` rust
pub enum Shutdown {
    Read,
    Write,
    Both,
}
```
the doc:

```

Read
The reading portion of the TcpStream should be shut down.

All currently blocked and future reads will return Ok(0).

Write
The writing portion of the TcpStream should be shut down.

All currently blocked and future writes will return an error.

Both
Both the reading and the writing portions of the TcpStream should be shut down.

See Shutdown::Read and Shutdown::Write for more information.
```
