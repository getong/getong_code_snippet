# rust iterator

## map
``` rust
let x_vec: Vec<char> = (b'a' ..= b'z').map(|x| x as char).collect();

let x_str: String = (b'a' ..= b'z').map(|x| x as char).collect();
```
