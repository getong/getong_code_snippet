# rust pin

[Rust的Pin与Unpin](https://folyd.com/blog/rust-pin-unpin/)
[Rust Pin 进阶](https://folyd.com/blog/rust-pin-advanced/)
[关于Pin,好绕的概念](https://rustcc.cn/article?id=4479f801-d28d-40cb-906c-85d8a04e8679)
[Self Referential Structs in Rust](https://arunanshub.hashnode.dev/self-referential-structs-in-rust)
[005 Rust 异步编程，Pin 介绍(a very basic example, but very helpful)](https://learnku.com/articles/46094)
[Pinning](http://www.tutzip.com/tut/rust-async-cn/04_pinning/01_chapter.zh.html)
[Pin and suffering](https://fasterthanli.me/articles/pin-and-suffering)
[Rust中的Pin详解](https://cloud.tencent.com/developer/article/1628311)

## related crate
[pin-utils](https://crates.io/crates/pin-utils)
[pin-project](https://crates.io/crates/pin-project)

## pin get element as mut

``` shell
struct SlowRead<R> {
    //       👇 now optional!
    reader: Option<R>,
    sleep: Sleep,
}

impl<R> SlowRead<R>
where
    R: Unpin,
{
    // 👇 now takes pinned mutable reference to Self, and returns an option
    fn take_inner(self: Pin<&mut Self>) -> Option<R> {
        self.reader.take()
    }
}
```
It does not compile.

``` rust
impl<R> SlowRead<R>
where
    R: Unpin,
{
    // 👇 now takes pinned mutable reference to Self, and returns an option
    fn take_inner(self: Pin<&mut Self>) -> Option<R> {
        unsafe { self.get_unchecked_mut().reader.take() }
    }
}

```
copy from [Pin and suffering](https://fasterthanli.me/articles/pin-and-suffering)

## rust api doc

|method name                | meaning                     |
|:--------------------------| :------------:              |
|new()                      |pin a Unpin value            |
|unsafe new_unchecked()     |pin a !Unpin value           |
|as_ref()                   |convert &Pin<P<T>> to Pin<&T>|
|as_mut()                   |convert &mut Pin<P<T>> to Pin<&mut T>|
|get_ref()                  |convert Pin<P<T>> to &T      |
|get_mut()                  |convert Pin<P<T>>, T:Unpin to &mut T      |
|unsafe get_unchecked_mut() |convert Pin<P<T>>, T:!Unpin to &mut T      |
|Pin::into_inner(pin)       |convert Pin<p<t>> to p, t:Unpin          |
|unsafe Pin::into_inner_unchecked(pin)       |convert Pin<p<t>> to p, t: !Unpin|
|set(t)                     | set a new value t to the old t|
|into_ref()                 | convert Pin<&mut T> to Pin<&T>|
|unsafe map_unchecked(func: F)     | Constructs a new pin by mapping the interior value.|
|unsafe map_unchecked_mut(func: F)     | Constructs a new pin by mapping the interior value.|

copy from [Rust Pin 进阶](https://folyd.com/blog/rust-pin-advanced/)

## mutable pinned

``` rust
struct SlowRead{
    reader: Option<R>,
}

let self : Pin<&mut SlowRead<R>> = ...
let this = unsafe{self.get_unchecked_mut()};
reader = &mut this.reader;

match reader{
    Some(reader) => {
        let reader = unsafe {Pin::new_unchecked(reader)};
        reader.poll_read(cx, buf)
        }
    None => {}
}
```
copy from [Pin and suffering](https://fasterthanli.me/articles/pin-and-suffering)
