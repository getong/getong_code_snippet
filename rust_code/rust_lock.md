# rust lock

## RwLock can not work with std::sync::mpsc::Sender

``` rust
impl<T: ?Sized + Send> Send for RwLock<T>
impl<T: ?Sized + Send + Sync> Sync for RwLock<T>

impl<T: ?Sized + Send> Send for Mutex<T>
impl<T: ?Sized + Send> Sync for Mutex<T>

impl<T: Send> Send for Sender<T>
impl<T> !Sync for Sender<T>

impl<T: ?Sized + Sync + Send> Send for Arc<T>
impl<T: ?Sized + Sync + Send> Sync for Arc<T>
```
Sender<T> is not Sync.
Mutex<T> is both Send and Sync if T inside of it is Send.
RwLock<T> is Send where T is Send, but it is only Sync if T is both Send + Sync.

copy from [Understanding Rust Thread Safety](https://onesignal.com/blog/thread-safety-rust/)

>>>
RwLock is Send only if the underlying value is Send (and same for Sync). Arc is Sync if the underlying value is Send.

copy from [Moving Arc<RwLock<T>> to threads leads to errors regarding missing Send trait](https://users.rust-lang.org/t/moving-arc-rwlock-t-to-threads-leads-to-errors-regarding-missing-send-trait/66055/2)

## Sender is not sync, but can move into

``` rust
.and_then(move |form, my_sender| async move {
    Ok::<_, Infallible>(my_handlers::handler_2(form, my_sender).await)
})
```
>>>
Sender is not Sync, so it cannot be shared, but it is Send, so you can move it into the async task.

The problem with your code is that the closure is capturing its environment by reference, even though the async block inside is move. You should just need to make the closure move too:

copy from [How to pass a std::sync::mpsc::Sender<T> to a handler in warp?](https://stackoverflow.com/questions/63432657/how-to-pass-a-stdsyncmpscsendert-to-a-handler-in-warp)
