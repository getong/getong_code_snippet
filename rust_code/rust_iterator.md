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

## IntoIterator

``` rust
trait IntoIterator where Self::IntoIter: Iterator<Item=Self::Item> {
    type Item;
    type IntoIter: Iterator;
    fn into_iter(self) -> Self::IntoIter;
}
``
The doc:

```
Most collections actually provide several implementations of
IntoIterator, for shared references (&T), mutable references (&mut T),
and moves (T):

    Given a shared reference to the collection, into_iter returns an
iterator that produces shared references to its items. For example,
in the preceding code, (&favorites).into_iter() would return
an iterator whose Item type is &String.

    Given a mutable reference to the collection, into_iter returns an
iterator that produces mutable references to the items. For example,
if vector is some Vec<String>, the call (&mut
vector).into_iter() returns an iterator whose Item type is
&mut String.

    When passed the collection by value, into_iter returns an iterator
that takes ownership of the collection and returns items by value;
the items’ ownership moves from the collection to the consumer,
and the original collection is consumed in the process. For example, the call favorites.into_iter() in the preceding code returns an iterator that produces each string by value; the consumer
receives ownership of each string. When the iterator is dropped,
any elements remaining in the BTreeSet are dropped too, and the
set’s now-empty husk is disposed of.

    Since a for loop applies IntoIterator::into_iter to its operand, these
three implementations are what create the following idioms for iterating
over shared or mutable references to a collection, or consuming the collection and taking ownership of its elements:

    for element in &collection { ... }
    for element in &mut collection { ... }
    for element in collection { ... }
```

## c_string

``` rust
fn c_string(bytes: &[u8]) -> Option<&str> {
    let bytes_without_null = match bytes.iter().position(|&b| b == 0) {
        Some(ix) => &bytes[..ix],
        None => bytes,
    };

    std::str::from_utf8(bytes_without_null).ok()
}
```
## iterator favor
do

``` rust
let slice = &[1,2,3,4];
for i in slice {
}
```
do not

``` rust
for slice = &[1,2,3,4];
for i in 0 .. slice.len() {
}
```
