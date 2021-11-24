# rust tricks

## Better Looking Numbers

``` rust
let numberval: u64 = 12_000_000;
```

## Swapping

``` rust
use std::mem;

let mut x = 5;
let mut y = 42;

mem::swap(&mut x, &mut y);

assert_eq!(42, x);
assert_eq!(5, y);
```

## drop

``` rust
let v = vec![1, 2, 3];

drop(v); // explicitly drop the vector
```

## Print Debugging

``` rust
fn foo(n: usize) {
    if let Some(_) = dbg!(n.checked_sub(4)) {
        // ...
    }
}

foo(3)
```

copy from [Rust tricks I wish I knew earlier](https://preettheman.medium.com/rust-tricks-i-wish-i-knew-earlier-a39e2c214ecf)
