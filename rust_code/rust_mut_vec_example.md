# rust mut vec example

## intersecting lifetimes

``` rust
let mut a = vec![];
let b = &mut a;
b.push(0);
let c = &mut a;
c.push(1);
```

## part of struct

``` rust
struct S(u32, u32);
let mut a = S(0, 1);
let b = &mut a.0;
let c = &mut a.1;
*b = 2;
*c = 3;
```

## mut borrow at the same time

``` rust
let mut a = vec![0, 1];
let v = if let [first, second] = &mut a[..] {
  Some((first, second))
} else {
  None
};
if let Some((b, c)) = v {
  *b = 2;
  *c = 3;
}
```

## split_at_mut

``` rust
// returns two mutable references, one on the part before position 'mid', the other after it:
vec.split_at_mut(mid)
// like before, but the first part contains the first element only:
vec.split_first_mut()
// like before, but the second part contains the last element only:
vec.split_last_mut()

let mut a = vec![0, 1, 2];
if let Some((b, rest)) = a.split_first_mut(){
  *b = 3;
  let c = rest.get_mut(0).unwrap();
  *c = 4;
}
```

## std::mem::take

``` rust
let mut a = vec![0, 1];
let b = std::mem::take(&mut a);
println!("{:?}", a); // []    <--- !!!
println!("{:?}", b); // [0,1]

// like the code below:
let mut a = vec![0, 1];
let b = a;
a = vec![];
```

and the function usage:

``` rust
let mut a = vec![0, 1];
fn f(v: &mut Vec<u32>) {
  let b = std::mem::take(v);
  println!("{:?}", b); // [0, 1]
}
f(&mut a);
println!("{:?}", a); // []
```

## std::mem::replace

``` rust
let mut a = vec![0, 1];
let c = vec![3, 4];
let b = std::mem::replace(&mut a, c);
println!("{:?}", b); // [0, 1]
println!("{:?}", a); // [3, 4]
```

## std::mem::replace() with mut borrow object

``` rust
let mut a = &mut vec![0, 1];
let mut c = vec![3, 4];
let b = std::mem::replace(&mut a, &mut c);
println!("{:?}", b); // [0, 1]
println!("{:?}", a); // [3, 4]
```

## final code example

``` rust
struct A_Iter<'a> {
    v: &'a mut [u32],
}

impl<'a> Iterator for A_Iter<'a> {
    type Item = &'a mut u32;
    fn next(&mut self) -> Option<Self::Item> {
      let v_tmp = std::mem::replace(&mut self.v, &mut []);
      return if let Some((e, rest)) = v_tmp.split_first_mut() {
        self.v = rest;
        Some(e)
      } else {
        None
      };
    }
}
```


copy from [Mutable References on Vectors vs. Structs: Some less known techniques.](https://applied-math-coding.medium.com/mutable-references-on-vectors-vs-structs-some-less-known-techniques-87098e2e2ba2)
