# rust HashMap

## update HashMap element

``` rust
let mut map = HashMap::new();
map["key"] = "value";

*my_map.get_mut("a").unwrap() += 10;

*my_map.entry("a").or_insert(42) += 10;
```
copy from [How can I update a value in a mutable HashMap?](https://stackoverflow.com/questions/30414424/how-can-i-update-a-value-in-a-mutable-hashmap)


## hashmap literal

``` rust
use std::collections::HashMap;
macro_rules! map(
    { $($key:expr => $value:expr),+ } => {
        {
            let mut m = ::std::collections::HashMap::new();
            $(
                m.insert($key, $value);
            )+
            m
        }
     };
);

fn main() {
    let names = map!{ 1 => "one", 2 => "two" };
    println!("{} -> {:?}", 1, names.get(&1));
    println!("{} -> {:?}", 10, names.get(&10));
}
```
copy from [How do I create a HashMap literal?](https://stackoverflow.com/questions/27582739/how-do-i-create-a-hashmap-literal)
