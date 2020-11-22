# rust PhantomData

## unused lifetime

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

``` rust
use std::marker;

struct Vec<T> {
    data: *const T, // *const for covariance!
    len: usize,
    cap: usize,
    _marker: marker::PhantomData<T>,
}
```
copy from [Phantom Data](https://www.jianshu.com/p/8554bbf13a02)
