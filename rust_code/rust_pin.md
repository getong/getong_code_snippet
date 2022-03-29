# rust pin

[Rust的Pin与Unpin](https://folyd.com/blog/rust-pin-unpin/)
[Self Referential Structs in Rust](https://arunanshub.hashnode.dev/self-referential-structs-in-rust)
[005 Rust 异步编程，Pin 介绍(a very basic example, but very helpful)](https://learnku.com/articles/46094)

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
