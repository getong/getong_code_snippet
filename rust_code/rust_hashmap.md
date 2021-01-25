# rust HashMap

## update HashMap element

``` rust
let mut map = HashMap::new();
map["key"] = "value";

*my_map.get_mut("a").unwrap() += 10;

*my_map.entry("a").or_insert(42) += 10;
```
copy from [How can I update a value in a mutable HashMap?](https://stackoverflow.com/questions/30414424/how-can-i-update-a-value-in-a-mutable-hashmap)
