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

## read_payload

``` rust
    pub fn read_payload(&self) -> Box<[u8]> {
        //  self.cursor.remaining_slice().to_vec().into_boxed_slice()
        self.buffer[self.cursor.position() as usize..self.buffer.len()]
            .to_vec()
            .into_boxed_slice()
    }
```

## read

``` rust
use byteorder::{BigEndian, ReadBytesExt, WriteBytesExt};
    fn read(rdr: &mut Cursor<&[u8]>) -> Self::Header {
        let protocol_version = rdr.read_u16::<BigEndian>()?; /* protocol id */
        let packet_id = rdr.read_u8()?;
        let delivery_guarantee_id = rdr.read_u8()?;
        let order_guarantee_id = rdr.read_u8()?;

        let header = StandardHeader {
            protocol_version,
            packet_type: PacketType::try_from(packet_id)?,
            delivery_guarantee: DeliveryGuarantee::try_from(delivery_guarantee_id)?,
            ordering_guarantee: OrderingGuarantee::try_from(order_guarantee_id)?,
        };

        Ok(header)
    }
```
