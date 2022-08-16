# rust stream


## AsyncRead and AsyncWrite
```
use futures::prelude::*;
use runtime::fs::File;

let f = file::create("foo.txt").await?; // create a file
f.write_all(b"hello world").await?;     // write data to the file (AsyncWrite)

let f = file::open("foo.txt").await?; // open a file
let mut buffer = Vec::new();          // init the buffer to read the data into
f.read_to_end(&mut buffer).await?;    // read the whole file (AsyncRead)
```

## Streams and Roles

让我们首先列举可以在典型系统中表达的各种流：

source：可以生成数据的流
Sink：可以消费数据的流
Through：消费数据，对其进行操作然后生成新数据的流
Duplex：流可以生成数据，也可以独立的消费数据
copy from [Rust流(Streams)](https://zhuanlan.zhihu.com/p/70247995)

## rust tokio trait bounds were not satisfied on forward method
>>>
The most telling lines of that error message are the following:

   | doesn't satisfy `_: warp::Stream`
   | doesn't satisfy `tokio::sync::mpsc::UnboundedReceiver<_>: StreamExt`
The forward method is defined in the StreamExt trait; due to a blanket implementation, anything that implements Stream also implements StreamExt. However, as of Tokio v1.6.0, UnboundedReceiver no longer implements Stream. The documentation instead states:

This receiver can be turned into a Stream using UnboundedReceiverStream.

Hence:

let (client_ws_sender, mut client_ws_rcv) = ws.split();
let (client_sender, client_rcv) = mpsc::unbounded_channel();
let client_rcv = UnboundedReceiverStream::new(client_rcv);  // <-- this

tokio::task::spawn(client_rcv.forward(client_ws_sender).map(|result| {
    if let Err(e) = result {
        eprintln!("error sending websocket msg: {}", e);
    }
}));

copy from [rust tokio trait bounds were not satisfied on forward method](https://stackoverflow.com/questions/67602278/rust-tokio-trait-bounds-were-not-satisfied-on-forward-method)
