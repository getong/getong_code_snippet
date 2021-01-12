# os path

``` rust
use std::path::PathBuf;
fn main() {
    let mut hello = PathBuf::from("/tmp/abc.txt");
    hello.pop();
    hello.pop();
    hello.pop();
    println!("{:?}", hello.display());
}
```
output :

```
"/"
```
