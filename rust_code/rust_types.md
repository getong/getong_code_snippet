# rust types

## primitive types
```
array
bool
char
numeric types
slice
str 不可变、静态的字符串
String 可变、不定长的字符串
tuple
function
pointer
```

## numeric types

```
i8 u8
i16 u16
i32 u32
i64 i64
i128 u128 ## nightly-only
isize usize
f32 f64
```
see [primitive type](https://doc.rust-lang.org/std/primitive.array.html)
see [Primitive Types](https://doc.rust-lang.org/book/first-edition/primitive-types.html)


## string

``` shell
str 是不可变的字符串；
std::String 是可变的字符串；
std::ffi::CStr 用于表示由C分配、rust借用的C字符串；
std::ffi::CString 用于表示由rust分配、可以传递给C函数使用的C字符串；
std::ffi::OsStr 平台相关的字符串，具体看 rust/os_str.rs at master · rust-lang/rust · GitHub；
std::ffi::OsString 这个是上面的可变版本；
std::path::Path 用来表示路径，方法和普通字符串不一样，当然独立出来；
std::path::PathBuf 这是Path的可变版本；
总之普通字符串就用str和String，路径就用Path和PathBuf，其他是ffi才需要用到的。算是挺清晰的设计。
```
see [Rust为什么会有这么多字符串相似类型？](https://www.zhihu.com/question/30807740)


## rust 里的 & 不是指针，是borrow, 对应的 &mut 是mutable borrow.

## pointer
use with as and unsafe

``` rust
let x = 5;
let raw = &x as *const i32;

let points_at = unsafe { *raw };

println!("raw points at {}", points_at);
```

## Three pointer types: references, boxes and unsafe poointers.

``` rust
// References
&T
&mut T

// Boxes
let t = 12;
let b = Box::new(t);

// Raw Pointers
*mut T
*const T
```

## ?Sized, Sized, Unsize
?Sized includes Sized and Unsize
