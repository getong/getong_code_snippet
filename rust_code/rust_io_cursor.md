# rust io cursor

``` rust
use std::io::Cursor;
use std::io::prelude::*;
use std::io::SeekFrom;

let mut buff = Cursor::new(vec![1, 2, 3, 4, 5]);

assert_eq!(buff.position(), 0);

buff.seek(SeekFrom::Current(2)).unwrap();
assert_eq!(buff.position(), 2);

buff.seek(SeekFrom::Current(-1)).unwrap();
assert_eq!(buff.position(), 1);
```
