# rust vector type

## with_capacity

``` rust
let mut v = Vec::with_capacity(2);
assert_eq!(v.len(), 0);
assert_eq!(v.capacity(), 2);
```

## insert

``` rust
v.insert(3, 35); // insert the index 3 position with the value 35
```

## remove

``` rust
v.remove(1); // remove the second element
```

## pop

``` rust
assert_eq!(v.pop(), Some(35));
```
