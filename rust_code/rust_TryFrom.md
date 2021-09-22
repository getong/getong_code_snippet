# rust TryFrom trait

copy from [Trait std::convert::TryFrom](https://doc.rust-lang.org/stable/std/convert/trait.TryFrom.html)

``` rust
use std::convert::TryFrom;

let big_number = 1_000_000_000_000i64;
// Silently truncates `big_number`, requires detecting
// and handling the truncation after the fact.
let smaller_number = big_number as i32;
assert_eq!(smaller_number, -727379968);

// Returns an error because `big_number` is too big to
// fit in an `i32`.
let try_smaller_number = i32::try_from(big_number);
assert!(try_smaller_number.is_err());

// Returns `Ok(3)`.
let try_successful_smaller_number = i32::try_from(3);
assert!(try_successful_smaller_number.is_ok());
```
