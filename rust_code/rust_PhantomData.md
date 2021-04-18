# rust PhantomData

## unused lifetime
类型或生命周期逻辑上与一个结构体关联起来了，但是却不属于结构体的任何一个成员。这种情况对于生命周期尤为常见。
``` rust
use std::marker::PhantomData;

struct Slice<'a, T: 'a> {
    start: *const T,
    end: *const T,
    phantom: PhantomData<&'a T>,
}
```


## Unused Type

``` rust
pub struct RetryableSendCh<T, C: Sender<T>> {
    ch: C,
    name: &'static str,

    marker: PhantomData<T>,
}
```

## Ownership and Drop check
为了让 drop 检查器知道我们确实拥有 T 类型的值，也就是需要在销毁 Vec 的时候同时销毁 T，我们需要添加一个额外的 PhantomData。
``` rust
use std::marker;

struct Vec<T> {
    data: *const T, // *const for covariance!
    len: usize,
    cap: usize,
    _marker: marker::PhantomData<T>,
}
```
让裸指针拥有数据是一个很普遍的设计，以至于标准库为它自己创造了一个叫 Unique<T> 的组件，它可以：

封装一个 *const T 处理变性
包含一个 PhantomData
自动实现 Send/Sync，模拟和包含 T 时一样的行为
将指针标记为 NonZero 以便空指针优化

copy from [Phantom Data](https://www.jianshu.com/p/8554bbf13a02)
copy doc from [3.10 PhantomData（幽灵数据）](https://learnku.com/docs/nomicon/2018/310-phantom-data/4721)
