# rust SystemTime

## unix time

``` rust
use std::time::{SystemTime, UNIX_EPOCH};

let now = SystemTime::now().duration_since(UNIX_EPOCH).expect("Time went backwards");
println!("now :{} ", now.as_millis());
```

## get the current local time

``` rust
use chrono;

fn main() {
    println!("{:?}", chrono::offset::Local::now());
    println!("{:?}", chrono::offset::Utc::now());
}
```
copy from [How to get Timestamp of the current Date and time in Rust](https://stackoverflow.com/questions/57707966/how-to-get-timestamp-of-the-current-date-and-time-in-rust)
