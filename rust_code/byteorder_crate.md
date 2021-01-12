# rust byteorder

## add byteorder to Carge.toml

``` shell
cargo add byteorder
```

## byteorder usage example

``` rust
use byteorder::{BigEndian, LittleEndian, ReadBytesExt, WriteBytesExt};
use std::io::Cursor;

fn main() {
    let mut rdr = Cursor::new(vec![2, 5, 3, 0]);
    // Note that we use type parameters to indicate which kind of byte order
    // we want!
    assert_eq!(517, rdr.read_u16::<BigEndian>().unwrap());
    assert_eq!(3, rdr.read_u16::<LittleEndian>().unwrap());

    let mut wtr = vec![];
    wtr.write_u16::<LittleEndian>(517).unwrap();
    wtr.write_u16::<BigEndian>(768).unwrap();
    assert_eq!(wtr, vec![5, 2, 3, 0]);
}
```
copy from [Crate byteorder](https://docs.rs/byteorder)
