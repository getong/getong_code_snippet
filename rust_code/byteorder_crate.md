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

## trait definition

``` rust
pub trait ReadBytesExt: io::Read {
    #[inline]
    fn read_u8(&mut self) -> Result<u8> {
        let mut buf = [0; 1];
        self.read_exact(&mut buf)?;
        Ok(buf[0])
    }

    #[inline]
    fn read_i8(&mut self) -> Result<i8> {
        let mut buf = [0; 1];
        self.read_exact(&mut buf)?;
        Ok(buf[0] as i8)
    }
    ...
}

pub trait WriteBytesExt: io::Write {
    #[inline]
    fn write_u8(&mut self, n: u8) -> Result<()> {
        self.write_all(&[n])
    }
    #[inline]
    fn write_i8(&mut self, n: i8) -> Result<()> {
        self.write_all(&[n as u8])
    }
    ...
}
```
copy from byteorder/src/io.rs
