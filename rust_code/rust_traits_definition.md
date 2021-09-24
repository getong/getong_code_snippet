# rust traits definition

## AsMut
std::convert::AsMut
``` rust
 pub trait AsMut<T>
where
    T: ?Sized,
{
    fn as_mut(&mut self) -> &mut T;
}
```

## AsRef
std::convert::AsRef
``` rust
pub trait AsRef<T>
where
    T: ?Sized,
{
    fn as_ref(&self) -> &T;
}
```

## Borrow
std::borrow::Borrow

``` rust
pub trait Borrow<Borrowed>
where
    Borrowed: ?Sized,
{
    fn borrow(&self) -> &Borrowed;
}
```

## BorrowMut
std::borrow::BorrowMut

``` rust
pub trait BorrowMut<Borrowed>: Borrow<Borrowed>
where
    Borrowed: ?Sized,
{
    fn borrow_mut(&mut self) -> &mut Borrowed;
}
```

## BufRead
std::io::BufRead

``` rust
pub trait BufRead: Read {
    fn fill_buf(&mut self) -> Result<&[u8]>;
    fn consume(&mut self, amt: usize);

    fn has_data_left(&mut self) -> Result<bool> {
        self.fill_buf().map(|b| !b.is_empty())
    }

    fn read_until(&mut self, byte: u8, buf: &mut Vec<u8>) -> Result<usize> {
        read_until(self, byte, buf)
    }

    fn read_line(&mut self, buf: &mut String) -> Result<usize> {
        // Note that we are not calling the `.read_until` method here, but
        // rather our hardcoded implementation. For more details as to why, see
        // the comments in `read_to_end`.
        append_to_string(buf, |b| read_until(self, b'\n', b))
    }

    fn split(self, byte: u8) -> Split<Self>
    where
        Self: Sized,
    {
        Split { buf: self, delim: byte }
    }

    fn lines(self) -> Lines<Self>
    where
        Self: Sized,
    {
        Lines { buf: self }
    }
}
```

## Clone
std::clone::Clone

``` rust
pub trait Clone: Sized {
    fn clone(&self) -> Self;

    fn clone_from(&mut self, source: &Self) {
        *self = source.clone()
    }
}
```

## Debug
std::fmt::Debug

``` rust
pub trait Debug {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error>;
}
```

## Default
std::default::Default

``` rust
pub trait Default {
    fn default() -> Self;
}
```

## Deref
std::ops::Deref

``` rust
pub trait Deref {
    type Target: ?Sized;
    fn deref(&self) -> &Self::Target;
}
```

## DerefMut
std::ops::DerefMut

``` rust
pub trait DerefMut: Deref {
    fn deref_mut(&mut self) -> &mut Self::Target;
}
```

## Display
std::fmt::Display

``` rust
pub trait Display {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error>;
}
```

## DoubleEndedIterator
std::iter::DoubleEndedIterator

``` rust
pub trait DoubleEndedIterator: Iterator {
    fn next_back(&mut self) -> Option<Self::Item>;

    fn advance_back_by(&mut self, n: usize) -> Result<(), usize> {
        for i in 0..n {
            self.next_back().ok_or(i)?;
        }
        Ok(())
    }

    fn nth_back(&mut self, n: usize) -> Option<Self::Item> {
        self.advance_back_by(n).ok()?;
        self.next_back()
    }

    fn try_rfold<B, F, R>(&mut self, init: B, mut f: F) -> R
    where
        Self: Sized,
        F: FnMut(B, Self::Item) -> R,
        R: Try<Output = B>,
    {
        let mut accum = init;
        while let Some(x) = self.next_back() {
            accum = f(accum, x)?;
        }
        try { accum }
    }

    fn rfold<B, F>(mut self, init: B, mut f: F) -> B
    where
        Self: Sized,
        F: FnMut(B, Self::Item) -> B,
    {
        let mut accum = init;
        while let Some(x) = self.next_back() {
            accum = f(accum, x);
        }
        accum
    }

    fn rfind<P>(&mut self, predicate: P) -> Option<Self::Item>
    where
        Self: Sized,
        P: FnMut(&Self::Item) -> bool,
    {
        #[inline]
        fn check<T>(mut predicate: impl FnMut(&T) -> bool) -> impl FnMut((), T) -> ControlFlow<T> {
            move |(), x| {
                if predicate(&x) { ControlFlow::Break(x) } else { ControlFlow::CONTINUE }
            }
        }

        self.try_rfold((), check(predicate)).break_value()
    }
}
```

## Drop
std::ops::Drop

``` rust
pub trait Drop {
    fn drop(&mut self);
}
```

## Error
std::error::Error

``` rust
pub trait Error: Debug + Display {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        None
    }

    fn type_id(&self, _: private::Internal) -> TypeId
    where
        Self: 'static,
    {
        TypeId::of::<Self>()
    }


    fn cause(&self) -> Option<&dyn Error> {
        self.source()
    }
}
```

## ExactSizeIterator
std::iter::ExactSizeIterator

``` rust
pub trait ExactSizeIterator: Iterator {
    fn len(&self) -> usize {
        let (lower, upper) = self.size_hint();
        assert_eq!(upper, Some(lower));
        lower
    }

    fn is_empty(&self) -> bool {
        self.len() == 0
    }
}
```

## Extend
std::iter::Extend

``` rust
pub trait Extend<A> {
    fn extend<T>(&mut self, iter: T)
    where
        T: IntoIterator<Item = A>;

    fn extend_one(&mut self, item: A) {
        self.extend(Some(item));
    }

    fn extend_reserve(&mut self, additional: usize) {
        let _ = additional;
    }
}
```

## Fn
std::ops::Fn

``` rust
pub trait Fn<Args>: FnMut<Args> {
    extern "rust-call" fn call(&self, args: Args) -> Self::Output;
}
```

## FnMut
std::ops::FnMut

``` rust
pub trait FnMut<Args>: FnOnce<Args> {
    extern "rust-call" fn call_mut(
        &mut self,
        args: Args
    ) -> Self::Output;
}
```

## FnOnce
std::ops::FnOnce

``` rust
pub trait FnOnce<Args> {
    type Output;
    extern "rust-call" fn call_once(self, args: Args) -> Self::Output;
}
```

## From
std::convert::From

``` rust
pub trait From<T> {
    fn from(T) -> Self;
}
```

## FromIterator
std::iter::FromIterator
``` rust
pub trait FromIterator<A> {
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = A>;
}
```

## Future
std::future::Future

``` rust
pub trait Future {
    type Output;
    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output>;
}
```

## Generator
std::ops::Generator

``` rust
pub trait Generator<R = ()> {
    type Yield;
    type Return;
    fn resume(
        self: Pin<&mut Self>,
        arg: R
    ) -> GeneratorState<Self::Yield, Self::Return>;
}
```

## Hash
std::hash::Hash

``` rust
pub trait Hash {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher;

    fn hash_slice<H: Hasher>(data: &[Self], state: &mut H)
    where
        Self: Sized,
    {
        for piece in data {
            piece.hash(state);
        }
    }
}
```

## Hasher
std::hash::Hasher

``` rust
pub trait Hasher {
    fn finish(&self) -> u64;
    fn write(&mut self, bytes: &[u8]);

    fn write_u8(&mut self, i: u8) { ... }
    fn write_u16(&mut self, i: u16) { ... }
    fn write_u32(&mut self, i: u32) { ... }
    fn write_u64(&mut self, i: u64) { ... }
    fn write_u128(&mut self, i: u128) { ... }
    fn write_usize(&mut self, i: usize) { ... }
    fn write_i8(&mut self, i: i8) { ... }
    fn write_i16(&mut self, i: i16) { ... }
    fn write_i32(&mut self, i: i32) { ... }
    fn write_i64(&mut self, i: i64) { ... }
    fn write_i128(&mut self, i: i128) { ... }
    fn write_isize(&mut self, i: isize) { ... }
}
```

## Iterator
std::iter::Iterator

``` rust
pub trait Iterator {
    type Item;
    fn next(&mut self) -> Option<Self::Item>;

    fn size_hint(&self) -> (usize, Option<usize>) { ... }
    fn count(self) -> usize { ... }
    fn last(self) -> Option<Self::Item> { ... }
    fn advance_by(&mut self, n: usize) -> Result<(), usize> { ... }
    fn nth(&mut self, n: usize) -> Option<Self::Item> { ... }
    fn step_by(self, step: usize) -> StepBy<Self>ⓘ { ... }
    fn chain<U>(self, other: U) -> Chain<Self, <U as IntoIterator>::IntoIter>ⓘ
    where
        U: IntoIterator<Item = Self::Item>,
    { ... }
    fn zip<U>(self, other: U) -> Zip<Self, <U as IntoIterator>::IntoIter>ⓘ
    where
        U: IntoIterator,
    { ... }
    fn intersperse(self, separator: Self::Item) -> Intersperse<Self>ⓘ
    where
        Self::Item: Clone,
    { ... }
    fn intersperse_with<G>(self, separator: G) -> IntersperseWith<Self, G>ⓘ
    where
        G: FnMut() -> Self::Item,
    { ... }
    fn map<B, F>(self, f: F) -> Map<Self, F>ⓘ
    where
        F: FnMut(Self::Item) -> B,
    { ... }
    fn for_each<F>(self, f: F)
    where
        F: FnMut(Self::Item),
    { ... }
    fn filter<P>(self, predicate: P) -> Filter<Self, P>ⓘ
    where
        P: FnMut(&Self::Item) -> bool,
    { ... }
    fn filter_map<B, F>(self, f: F) -> FilterMap<Self, F>ⓘ
    where
        F: FnMut(Self::Item) -> Option<B>,
    { ... }
    fn enumerate(self) -> Enumerate<Self>ⓘ { ... }
    fn peekable(self) -> Peekable<Self>ⓘ { ... }
    fn skip_while<P>(self, predicate: P) -> SkipWhile<Self, P>ⓘ
    where
        P: FnMut(&Self::Item) -> bool,
    { ... }
    fn take_while<P>(self, predicate: P) -> TakeWhile<Self, P>ⓘ
    where
        P: FnMut(&Self::Item) -> bool,
    { ... }
    fn map_while<B, P>(self, predicate: P) -> MapWhile<Self, P>ⓘ
    where
        P: FnMut(Self::Item) -> Option<B>,
    { ... }
    fn skip(self, n: usize) -> Skip<Self>ⓘ { ... }
    fn take(self, n: usize) -> Take<Self>ⓘ { ... }
    fn scan<St, B, F>(self, initial_state: St, f: F) -> Scan<Self, St, F>ⓘ
    where
        F: FnMut(&mut St, Self::Item) -> Option<B>,
    { ... }
    fn flat_map<U, F>(self, f: F) -> FlatMap<Self, U, F>ⓘ
    where
        F: FnMut(Self::Item) -> U,
        U: IntoIterator,
    { ... }
    fn flatten(self) -> Flatten<Self>ⓘ
    where
        Self::Item: IntoIterator,
    { ... }
    fn fuse(self) -> Fuse<Self>ⓘ { ... }
    fn inspect<F>(self, f: F) -> Inspect<Self, F>ⓘ
    where
        F: FnMut(&Self::Item),
    { ... }
    fn by_ref(&mut self) -> &mut Self { ... }
    fn collect<B>(self) -> B
    where
        B: FromIterator<Self::Item>,
    { ... }
    fn partition<B, F>(self, f: F) -> (B, B)
    where
        F: FnMut(&Self::Item) -> bool,
        B: Default + Extend<Self::Item>,
    { ... }
    fn partition_in_place<'a, T, P>(self, predicate: P) -> usize
    where
        Self: DoubleEndedIterator<Item = &'a mut T>,
        T: 'a,
        P: FnMut(&T) -> bool,
    { ... }
    fn is_partitioned<P>(self, predicate: P) -> bool
    where
        P: FnMut(Self::Item) -> bool,
    { ... }
    fn try_fold<B, F, R>(&mut self, init: B, f: F) -> R
    where
        F: FnMut(B, Self::Item) -> R,
        R: Try<Output = B>,
    { ... }
    fn try_for_each<F, R>(&mut self, f: F) -> R
    where
        F: FnMut(Self::Item) -> R,
        R: Try<Output = ()>,
    { ... }
    fn fold<B, F>(self, init: B, f: F) -> B
    where
        F: FnMut(B, Self::Item) -> B,
    { ... }
    fn reduce<F>(self, f: F) -> Option<Self::Item>
    where
        F: FnMut(Self::Item, Self::Item) -> Self::Item,
    { ... }
    fn all<F>(&mut self, f: F) -> bool
    where
        F: FnMut(Self::Item) -> bool,
    { ... }
    fn any<F>(&mut self, f: F) -> bool
    where
        F: FnMut(Self::Item) -> bool,
    { ... }
    fn find<P>(&mut self, predicate: P) -> Option<Self::Item>
    where
        P: FnMut(&Self::Item) -> bool,
    { ... }
    fn find_map<B, F>(&mut self, f: F) -> Option<B>
    where
        F: FnMut(Self::Item) -> Option<B>,
    { ... }
    fn try_find<F, R, E>(&mut self, f: F) -> Result<Option<Self::Item>, E>
    where
        F: FnMut(&Self::Item) -> R,
        R: Try<Output = bool, Residual = Result<Infallible, E>> + Try,
    { ... }
    fn position<P>(&mut self, predicate: P) -> Option<usize>
    where
        P: FnMut(Self::Item) -> bool,
    { ... }
    fn rposition<P>(&mut self, predicate: P) -> Option<usize>
    where
        Self: ExactSizeIterator + DoubleEndedIterator,
        P: FnMut(Self::Item) -> bool,
    { ... }
    fn max(self) -> Option<Self::Item>
    where
        Self::Item: Ord,
    { ... }
    fn min(self) -> Option<Self::Item>
    where
        Self::Item: Ord,
    { ... }
    fn max_by_key<B, F>(self, f: F) -> Option<Self::Item>
    where
        F: FnMut(&Self::Item) -> B,
        B: Ord,
    { ... }
    fn max_by<F>(self, compare: F) -> Option<Self::Item>
    where
        F: FnMut(&Self::Item, &Self::Item) -> Ordering,
    { ... }
    fn min_by_key<B, F>(self, f: F) -> Option<Self::Item>
    where
        F: FnMut(&Self::Item) -> B,
        B: Ord,
    { ... }
    fn min_by<F>(self, compare: F) -> Option<Self::Item>
    where
        F: FnMut(&Self::Item, &Self::Item) -> Ordering,
    { ... }
    fn rev(self) -> Rev<Self>ⓘ
    where
        Self: DoubleEndedIterator,
    { ... }
    fn unzip<A, B, FromA, FromB>(self) -> (FromA, FromB)
    where
        Self: Iterator<Item = (A, B)>,
        FromA: Default + Extend<A>,
        FromB: Default + Extend<B>,
    { ... }
    fn copied<'a, T>(self) -> Copied<Self>ⓘ
    where
        Self: Iterator<Item = &'a T>,
        T: 'a + Copy,
    { ... }
    fn cloned<'a, T>(self) -> Cloned<Self>ⓘ
    where
        Self: Iterator<Item = &'a T>,
        T: 'a + Clone,
    { ... }
    fn cycle(self) -> Cycle<Self>ⓘ
    where
        Self: Clone,
    { ... }
    fn sum<S>(self) -> S
    where
        S: Sum<Self::Item>,
    { ... }
    fn product<P>(self) -> P
    where
        P: Product<Self::Item>,
    { ... }
    fn cmp<I>(self, other: I) -> Ordering
    where
        I: IntoIterator<Item = Self::Item>,
        Self::Item: Ord,
    { ... }
    fn cmp_by<I, F>(self, other: I, cmp: F) -> Ordering
    where
        F: FnMut(Self::Item, <I as IntoIterator>::Item) -> Ordering,
        I: IntoIterator,
    { ... }
    fn partial_cmp<I>(self, other: I) -> Option<Ordering>
    where
        I: IntoIterator,
        Self::Item: PartialOrd<<I as IntoIterator>::Item>,
    { ... }
    fn partial_cmp_by<I, F>(self, other: I, partial_cmp: F) -> Option<Ordering>
    where
        F: FnMut(Self::Item, <I as IntoIterator>::Item) -> Option<Ordering>,
        I: IntoIterator,
    { ... }
    fn eq<I>(self, other: I) -> bool
    where
        I: IntoIterator,
        Self::Item: PartialEq<<I as IntoIterator>::Item>,
    { ... }
    fn eq_by<I, F>(self, other: I, eq: F) -> bool
    where
        F: FnMut(Self::Item, <I as IntoIterator>::Item) -> bool,
        I: IntoIterator,
    { ... }
    fn ne<I>(self, other: I) -> bool
    where
        I: IntoIterator,
        Self::Item: PartialEq<<I as IntoIterator>::Item>,
    { ... }
    fn lt<I>(self, other: I) -> bool
    where
        I: IntoIterator,
        Self::Item: PartialOrd<<I as IntoIterator>::Item>,
    { ... }
    fn le<I>(self, other: I) -> bool
    where
        I: IntoIterator,
        Self::Item: PartialOrd<<I as IntoIterator>::Item>,
    { ... }
    fn gt<I>(self, other: I) -> bool
    where
        I: IntoIterator,
        Self::Item: PartialOrd<<I as IntoIterator>::Item>,
    { ... }
    fn ge<I>(self, other: I) -> bool
    where
        I: IntoIterator,
        Self::Item: PartialOrd<<I as IntoIterator>::Item>,
    { ... }
    fn is_sorted(self) -> bool
    where
        Self::Item: PartialOrd<Self::Item>,
    { ... }
    fn is_sorted_by<F>(self, compare: F) -> bool
    where
        F: FnMut(&Self::Item, &Self::Item) -> Option<Ordering>,
    { ... }
    fn is_sorted_by_key<F, K>(self, f: F) -> bool
    where
        F: FnMut(Self::Item) -> K,
        K: PartialOrd<K>,
    { ... }
}
```

## Ord
std::cmp::Ord

``` rust
pub trait Ord: Eq + PartialOrd<Self> {
    fn cmp(&self, other: &Self) -> Ordering;

    fn max(self, other: Self) -> Self { ... }
    fn min(self, other: Self) -> Self { ... }
    fn clamp(self, min: Self, max: Self) -> Self { ... }
}
```

## PartialEq
std::cmp::PartialEq

``` rust
pub trait PartialEq<Rhs = Self>
where
    Rhs: ?Sized,
{
    fn eq(&self, other: &Rhs) -> bool;

    fn ne(&self, other: &Rhs) -> bool { ... }
}
```

## PartialOrd
std::cmp::PartialOrd

``` rust
pub trait PartialOrd<Rhs = Self>: PartialEq<Rhs>
where
    Rhs: ?Sized,
{
    fn partial_cmp(&self, other: &Rhs) -> Option<Ordering>;

    fn lt(&self, other: &Rhs) -> bool { ... }
    fn le(&self, other: &Rhs) -> bool { ... }
    fn gt(&self, other: &Rhs) -> bool { ... }
    fn ge(&self, other: &Rhs) -> bool { ... }
}
```

## Pointer
std::fmt::Pointer

``` rust
pub trait Pointer {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error>;
}
```

## Read
std::io::Read

``` rust
pub trait Read {
    fn read(&mut self, buf: &mut [u8]) -> Result<usize>;

    fn read_vectored(&mut self, bufs: &mut [IoSliceMut<'_>]) -> Result<usize> { ... }
    fn is_read_vectored(&self) -> bool { ... }
    unsafe fn initializer(&self) -> Initializer { ... }
    fn read_to_end(&mut self, buf: &mut Vec<u8>) -> Result<usize> { ... }
    fn read_to_string(&mut self, buf: &mut String) -> Result<usize> { ... }
    fn read_exact(&mut self, buf: &mut [u8]) -> Result<()> { ... }
    fn by_ref(&mut self) -> &mut Self
    where
        Self: Sized,
    { ... }
    fn bytes(self) -> Bytes<Self>ⓘ
    where
        Self: Sized,
    { ... }
    fn chain<R: Read>(self, next: R) -> Chain<Self, R>ⓘ
    where
        Self: Sized,
    { ... }
    fn take(self, limit: u64) -> Take<Self>ⓘ
    where
        Self: Sized,
    { ... }
}
```

## Seek
std::io::Seek

``` rust
pub trait Seek {
    fn seek(&mut self, pos: SeekFrom) -> Result<u64>;

    fn rewind(&mut self) -> Result<()> { ... }
    fn stream_len(&mut self) -> Result<u64> { ... }
    fn stream_position(&mut self) -> Result<u64> { ... }
}
```

## Stream
std::stream::Stream

``` rust
pub trait Stream {
    type Item;
    fn poll_next(
        self: Pin<&mut Self>,
        cx: &mut Context<'_>
    ) -> Poll<Option<Self::Item>>;

    fn size_hint(&self) -> (usize, Option<usize>) { ... }
}
```

## TryFrom
std::convert::TryFrom

``` rust
pub trait TryFrom<T> {
    type Error;
    fn try_from(value: T) -> Result<Self, Self::Error>;
}
```

## Write
std::io::Write

``` rust
pub trait Write {
    fn write(&mut self, buf: &[u8]) -> Result<usize>;
    fn flush(&mut self) -> Result<()>;

    fn write_vectored(&mut self, bufs: &[IoSlice<'_>]) -> Result<usize> { ... }
    fn is_write_vectored(&self) -> bool { ... }
    fn write_all(&mut self, buf: &[u8]) -> Result<()> { ... }
    fn write_all_vectored(&mut self, bufs: &mut [IoSlice<'_>]) -> Result<()> { ... }
    fn write_fmt(&mut self, fmt: Arguments<'_>) -> Result<()> { ... }
    fn by_ref(&mut self) -> &mut Self
    where
        Self: Sized,
    { ... }
}
```

## CoerceUnsized
std::ops::CoerceUnsized

``` rust
pub trait CoerceUnsized<T>
where
    T: ?Sized,
{ }
```

## RefUnwindSafe
std::panic::RefUnwindSafe

``` rust
pub auto trait RefUnwindSafe { }
```

## Send
std::marker::Send

``` rust
pub unsafe auto trait Send { }
```

## Sync
std::marker::Sync

``` rust
pub unsafe auto trait Sync { }
```

## UnwindSafe
std::panic::UnwindSafe

``` rust
pub auto trait UnwindSafe { }
```

## Any
std::any::Any

``` rust
pub trait Any: 'static {
    fn type_id(&self) -> TypeId;
}
```
