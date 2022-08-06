# rust vector type

## with_capacity

``` rust
let mut v = Vec::with_capacity(2);
assert_eq!(v.len(), 0);
assert_eq!(v.capacity(), 2);

// initialize the four elements
let v1 = vec![1,2,3,4];
// initialize 10 elements, each element is 0
let v2 = vec![0;10];
println!("{}", v1);
println!("{}", v2);
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

## swap_remove

``` rust
v.swap_remove(1);
std::mem::replace(&mut v[2], 123);
```

## get is just like [], but return Result type

``` rust
assert_eq!(v.get(0), Some(1));
assert_eq!(v[0], 1);
```

## slice.to_vec() method

``` rust
fn fill_vec(vec: &Vec<i32>) -> Vec<i32> {
    let mut vec: Vec<i32> = vec.to_vec();

    vec.push(22);
    vec.push(44);
    vec.push(66);

    vec

```

## multi dimansion vector

``` rust
let mut x = [[[23; 4]; 8]; 15];
x[14][7][3] = 56;
print!("{}, {}", x[0][0][0], x[14][7][3]);
```
Actually, only the usize type is allowed as an index of an array.

## as char

``` rust
for n in 32..127 {
println!("{}: [{}]", n, n as u8 as char);
}
for n in 160..256 {
println!("{}: [{}]", n, n as u8 as char);
}
```
