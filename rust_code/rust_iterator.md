# rust iterator

## map
``` rust
let x_vec: Vec<char> = (b'a' ..= b'z').map(|x| x as char).collect();

let x_str: String = (b'a' ..= b'z').map(|x| x as char).collect();

```

## fold

``` rust
let res: Vec<char> = (b'a' ..= b'z').map(|x| x as char).fold(Vec::new(), |mut acc, ch| { acc.push(ch); acc});

let res: String = (b'a' ..= b'z').map(|x| x as char).fold(String::new(), |mut acc, ch| { acc.push(ch); acc});
```

## trait Iterator next method

``` rust
pub trait Iterator {
    type Item;
    fn next(&mut self) -> Option<Self::Item>;
}
```
