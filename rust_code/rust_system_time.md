# rust SystemTime

## unix time

``` rust
use std::time::{SystemTime, UNIX_EPOCH};

let now = SystemTime::now().duration_since(UNIX_EPOCH).expect("Time went backwards");
println!("now :{} ", now.as_millis());
```
