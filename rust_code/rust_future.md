# rust future

## future

```
trait Future {
    type Output;
    fn poll(self: Pin<&mut self>, ctx: &mut Context<'_>) -> Poll::(Self::Output);
}

最显著的不同在于函数的两个参数类型

self类型被定义为Pin的分装，这个封装简单的来说，就是允许我们创建不可移动的Future类型；所谓的不可移动对象，是指对象里面可以出现某个字段的指针指向对象的其它字段的情况；这对于实现async/await是至关重要的。
第二个参数用一个Context对象替换了简单的wake函数
我们之所以需要一个Context对象，是因为我们需要在其中存储哪一个Future对象被轮询执行了；这在复杂的多线程程序中是不可或缺的。

Context对象和Waker类型
Context的定义如下

pub struct Context<'a> {
    waker: &'a Waker,
    _marker: PhantomData<fn(&'a ()) -> &'a ()>,
}
它里面仅仅是封装了一个额外的Waker对象，以便用户定义的Future和底层的Executor进行通信，通知对方自身已经准备好了下一步对应的结果就可以返回给外部，其本身提供了clone操作，并且可以可以在多线程环境中移动和共享，因为它标记自己实现了Sync和Send

pub struct Waker {
    waker: RawWaker,
}
impl Unpin for Waker {}
unsafe impl Send for Waker {}
unsafe impl Sync for Waker {}
其中嵌套的内部实现里面封装了一个满足具体的Executor要求的、可以包含任意上下文数据的指针data，它的注释很好地阐述了数据的用途：

#[derive(PartialEq, Debug)]
pub struct RawWaker {
    /// A data pointer, which can be used to store arbitrary data as required
    /// by the executor. This could be e.g. a type-erased pointer to an `Arc`
    /// that is associated with the task.
    /// The value of this field gets passed to all functions that are part of
    /// the vtable as the first parameter.
    data: *const (),
    /// Virtual function pointer table that customizes the behavior of this waker.
    vtable: &'static RawWakerVTable,
}
Waker自身提供了wake()函数，和上面简化的例子类似

#[inline]
pub fn wake(self) {
    let wake = self.waker.vtable.wake;
    let data = self.waker.data;
    crate::mem::forget(self);
    unsafe { (wake)(data) };
}

Rust的await和async
这次加入1.36稳定版的功能体验在语言层面的两个关键字async和await。

async
async用于声明一个代码块为返回一个Future，通过在某个普通的函数声明前面加上async，Rust可以自动完成返回类型到Future类型的封装和转换，如下面的代码

async fn do_something() {
    //some heavy operation
}
的返回值会是一个Future.

Rust本身的Executor库也提供了阻塞执行的方法，允许我们在当前的调用线程里面阻塞执行直到封装的异步执行块允许完毕，即如下的代码

let fut = do_something();
block_on(fut);
// proceeds until wrapped something is executed
await
await语句可以作用在Future上，用于非阻塞方式的同步逻辑，即异步地等待作用的Future对象的完成，然后读取返回的结果，考虑如下的三个异步的Future执行块，前两个有先后依赖而第三个可以同时进行：

async fn learn_song() -> Song {
    //dom something
    Song
}
async fn sing_song(song: Song) {
    //sing the song
}

async fn dance() {
    //dance
}
可以用如下的逻辑来表述上面的并发执行行为

async learn_and_sing() {
    let song = learn_song().await;
    sing_song(song).await;
}

let f1 = learn_and_sing().await;
let f2 = dance();
futures::join(f1, f2);
标准库中的Future和futures crate
目前有两个Future库同时存在，一个是标准库中的std::future::Future，另外一个则是futures中定义的futures::future::Future。这一重复定义多少让人感到困惑不解：其实这主要是由于Future特性正在被开发中还不算足够完善的缘故。

早期的实现是通过future-rs扩展库的方式提供的，最近的版本才将它加到了标准库中；甚至于实现也是移过去的；可以认为std::future::Future实现了future-rs里面的一个最小集。后续的功能演进也可能仍然采用类似的策略

标准库中的Future进来保持最小的接口
更复杂的组合功能将会用类似future库的方å¼来提供
FutureExt
这是基于Future之上的一个扩展的Trait，可以实现很多方便的转换，包括

map将包含的输出值经过一个函数处理变换为另外一种输出
then实现两个Feature的链式操作，并且将前一个的输出传递为第二个闭包函数的输入，例如
let f1 = async {1};
let f2 = f1.then(|x| sync move { x + 3});
assert_eq!(f2.await, 4);
left_future/right_future实现根据不同的条件返回不同部分的EitherFuture功能，如
let x = 6;
let f = if x < 10 {
  async {true}.left_future()
} else {
  async {false}.right_future()
}
into_stream将future转换为包含单个元素的stream，这个stream的输出是future本身，支持方便的stream操作，如
let f = async {17};
let collected:Vec<_> = f.into_stream().collect().await;
flatten用于实现一次解封操作，等价于 f.then(|x| x)
inspect实现一个future传递之前的额外查看和处理，最简单的例子是打印计算的结果，如
let f = async {1};
let nf = f.inspect(|&x| println!("will resolve as {}", x)); //nf = 1
```

copy from [Rust语言的异步编程模型和协程支持](https://skyscribe.github.io/post/2019/12/07/rust-asynchronous-model-and-features/)

## async_std::task::yield_now

```
while computation_not_done() {
... do one medium-sized step of computation ...
async_std::task::yield_now().await;
}
```

## async_std::task::spawn_blocking

```
This function takes a closure, starts it running on its own thread, and returns a future of its return
value. Asynchronous code can await that future, yielding its thread to other
tasks until the computation is ready. By putting the hard work on a separate
thread, you can let the operating system take care of making it share the
processor nicely
```

## Unpin trait

``` rust
trait Unpin {}
```
almost all types in Rust automatically implement Unpin, using special support in the compiler.
Asynchronous function and block futres are the exceptions to this rule.


## cancel task

``` rust
async fn handle_connection(socket: TcpStream, channel: Channel) {
    let reader = Arc::new(socket);
    let writer = reader.clone();

    let read_task = task::spawn(async move {
        while let Some(line_in) in parse_line(&reader).await? {
            broadcast_line(line_in)?;
        }

        Ok(())
    });

    loop {
        // `channel` and JoinHandle are both "channel-like" types.
        select! {
            _ = read_task.join() => {
                // The connection closed or we encountered an error,
                // exit the loop
                break;
            }
            line_out = channel.recv() => {
                if write_line(&writer, line_out).await.is_err() {
                    read_task.abort();
                    read_task.join();
                }
            }
        }
    }
}
```
copy from [EXPLORING WAYS TO MAKE ASYNC RUST EASIER](https://carllerche.com/2021/06/17/six-ways-to-make-async-rust-easier/)

## tcpstream and future

``` rust
async fn parse_line(socket: &TcpStream) -> Result<String, Error> {
    let len = socket.read_u32().await?;
    let mut line = vec![0; len];
    socket.read_exact(&mut line).await?;
    let line = str::from_utf8(line)?;
    Ok(line)
}


async fn handle_connection(socket: TcpStream, channel: Channel) {
    let reader = Arc::new(socket);
    let writer = reader.clone();

    let read_task = task::spawn(async move {
        while let Some(line_in) in parse_line(&reader).await? {
            broadcast_line(line_in)?;
        }

        Ok(())
    });

    loop {
        // `channel` and JoinHandle are both "channel-like" types.
        select! {
            _ = read_task.join() => {
                // The connection closed or we encountered an error,
                // exit the loop
                break;
            }
            line_out = channel.recv() => {
                if write_line(&writer, line_out).await.is_err() {
                    read_task.abort();
                    read_task.join();
                }
            }
        }
    }
}


```

Or

``` Rust
#[abort_safe]
async fn read_line(&mut self) -> io::Result<Option<String>> {
    loop {
        // Consume a full line from the buffer
        if let Some(line) = self.parse_line()? {
            return Ok(line);
        }

        // Not enough data has been buffered to parse a full line
        if 0 == self.socket.read_buf(&mut self.buffer)? {
            // The remote closed the connection.
            if self.buffer.is_empty() {
                return Ok(None);
            } else {
                return Err("connection reset by peer".into());
            }
        }
    }
}

loop {
    select! {
        line_in = connection.read_line()? => {
            if let Some(line_in) = line_in {
                broadcast_line(line_in);
            } else {
                // connection closed, exit loop
                break;
            }
        }
        line_out = channel.recv() => {
            connection.write_line(line_out)?;
        }
    }
}
```
copy from [EXPLORING WAYS TO MAKE ASYNC RUST EASIER](https://carllerche.com/2021/06/17/six-ways-to-make-async-rust-easier/)

## LocalExecutor example

``` Rust
// This is spawned as a task in a `LocalExecutor`. `BattleManager` outlives
// the executor, so I can just send it in by reference 🤯
pub async fn battle(stream: Async<TcpStream>, manager: &BattleManager) {
    // `!Sync` read and write halves of WebSocket using a modified Soketto
    let server = UnsyncServer::new(stream);
    let (mut sender, mut receiver) = server.split();

    // `!Sync` read and write halves of a quasi-ring buffer.
    let (writer, mut reader) = new_shared();

    // We find a battle to put this socket into, and do just that.
    // Each battle instance is wrapped in `Rc<RefCell<_>>`.
    let battle = manager.matchmake();
    let cid = battle.borrow_mut().join(writer);

    // Loop handling outgoing messages turned into a simple future
    let outgoing = async move {
        while let Some(mut buf) = reader.read().await {
            if let Err(err) = sender.send(&mut buf[..]).await {
                log::error!("Connection error: {err:?}");
                break;
            }
            // `buf` is dropped here, which safely advances read head
        }

        let _ = sender.close().await;
    };

    // Loop handling incoming messages turned into a simple future
    let incoming = async move {
        let mut data = Vec::new();

        loop {
            data.clear();

            if receiver.receive_data(&mut data).await.is_err() {
                battle.borrow_mut().leave(cid);
                break;
            }

            let mut battle = battle.borrow_mut();

            // Process incoming messages
            for client_message in core::Decoder::new(&data) {
                battle.handle_message(cid, client_message);
            }

            // Broadcast all outgoing messages buffered for all clients
            battle.flush();
        }
    };

    // Zip (join) the two futures together so the two loops can run
    // concurrently. Yes sometimes I double-poll one, who cares.
    zip(incoming, outgoing).await;

    log::info!("Connection closed");
}
```
copy from [Local Async Executors and Why They Should be the Default](https://maciej.codes/2022-06-09-local-async.html)
