# tokio oneshot

## A channel for sending a `single` message between asynchronous tasks.
The oneshow sender can be used only once.
see the `send` method definition:

``` rust
pub fn send(mut self: Self, t: T) -> Result<(), T>
```

## use oneshot with loop method

``` rust
use tokio::sync::oneshot; //

fn main() {
    let (sender, receiver) = oneshot::channel::<u8>();
    tokio::spawn(async move {
        sender.send(3).unwrap();

        loop {
            // Do things
        }
    });
}
```
Or:

``` rust
use tokio::sync::oneshot;

fn main() {
    let (sender, receiver) = oneshot::channel::<u8>();
    tokio::spawn(async move {
        let mut sender = Some(sender);

        loop {
            if let Some(sender) = sender.take() {
                sender.send(3).unwrap();
            }
            // Do things
        }
    });
}
```
copy from [How do I use a Tokio oneshot sender and receiver on different tasks with inner loops?](https://stackoverflow.com/questions/66282294/how-do-i-use-a-tokio-oneshot-sender-and-receiver-on-different-tasks-with-inner-l)
