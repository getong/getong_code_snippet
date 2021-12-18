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
