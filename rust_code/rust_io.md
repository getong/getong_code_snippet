# rust io

## io module

``` rust
use std::io;


let stdin = io::stdin();
let reader = stdin.lock();
for line_ in reader.line() {
    let line = line_.unwrap();
    println!("line is {}", line);
}
```
