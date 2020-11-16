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
