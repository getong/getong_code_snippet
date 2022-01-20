# rust str

&str的类型不止一种。我们有。

字符串: 当你写let my_str = "I am a &str"的时候，你就会产生这些字符。它们在整个程序中持续存在，因为它们是直接写进二进制中的，它们的类型是 &'static str。'的意思是它的生命期，字符串字元有一个叫static的生命期。
借用str。这是常规的 &str 形式，没有 static 生命期。如果你创建了一个String，并得到了它的引用，当你需要它时，Rust会把它转换为&str。比如说

``` rust
fn prints_str(my_str: &str) { // it can use &String like a &str
    println!("{}", my_str);
}

fn main() {
    let my_string = String::from("I am a string");
    prints_str(&my_string); // we give prints_str a &String
}
```

copy from [&str的类型](https://kumakichi.github.io/easy_rust_chs/Chapter_39.html)

## char len_utf8() method

``` rust
>> '中'.len_utf8()
3
>> 'a'.len_utf8()
1
```

## How do I get a *mut c_char from a Str?

copy from [How do I get a *mut c_char from a Str?](https://stackoverflow.com/questions/28094636/how-do-i-get-a-mut-c-char-from-a-str)
``` rust
let string: &str = "Hello, world!";
let bytes: Vec<u8> = String::from(string).into_bytes();
let mut c_chars: Vec<i8> = bytes.iter().map(| c | *c as i8).collect::<Vec<i8>>();

c_chars.push(0); // null terminator

let ptr: *mut c_char = c_chars.as_mut_ptr();
```

## convert str to *mut *mut c_char

copy from [*mut T, *const T](https://cxx.rs/binding/rawptr.html)
``` rust
// src/main.rs

use std::env;
use std::ffi::CString;
use std::os::raw::c_char;
use std::os::unix::ffi::OsStrExt;
use std::ptr;

#[cxx::bridge]
mod ffi {
    extern "C++" {
        include!("example/include/args.h");

        unsafe fn parseArgs(argc: i32, argv: *mut *mut c_char);
    }
}

fn main() {
    // Convert from OsString to nul-terminated CString, truncating each argument
    // at the first inner nul byte if present.
    let args: Vec<CString> = env::args_os()
        .map(|os_str| {
            let bytes = os_str.as_bytes();
            CString::new(bytes).unwrap_or_else(|nul_error| {
                let nul_position = nul_error.nul_position();
                let mut bytes = nul_error.into_vec();
                bytes.truncate(nul_position);
                CString::new(bytes).unwrap()
            })
        })
        .collect();

    // Convert from Vec<CString> of owned strings to Vec<*mut c_char> of
    // borrowed string pointers.
    //
    // Once extern type stabilizes (https://github.com/rust-lang/rust/issues/43467)
    // and https://internals.rust-lang.org/t/pre-rfc-make-cstr-a-thin-pointer/6258
    // is implemented, and CStr pointers become thin, we can sidestep this step
    // by accumulating the args as Vec<Box<CStr>> up front, then simply casting
    // from *mut [Box<CStr>] to *mut [*mut CStr] to *mut *mut c_char.
    let argc = args.len();
    let mut argv: Vec<*mut c_char> = Vec::with_capacity(argc + 1);
    for arg in &args {
        argv.push(arg.as_ptr() as *mut c_char);
    }
    argv.push(ptr::null_mut()); // Nul terminator.

    unsafe {
        ffi::parseArgs(argc as i32, argv.as_mut_ptr());
    }

    // The CStrings go out of scope here. C function must not have held on to
    // the pointers beyond this point.
}
```

## convert *mut *mut c_char to CStr
[Foreign Function Interface (FFI)](https://anssi-fr.github.io/rust-guide/07_ffi.html)

``` rust
//! A direct way to access environment variables (on Unix).
//! Should not be used! Not thread safe, have a look at `std::env`!

extern {
    // Libc global variable
    #[link_name = "environ"]
    static libc_environ: *const *const std::os::raw::c_char;
}

fn main() {
    let mut next = unsafe { libc_environ };
    while !next.is_null() && !unsafe { *next }.is_null() {
        let env = unsafe { std::ffi::CStr::from_ptr(*next) }
            .to_str()
            .unwrap_or("<invalid>");
        println!("{}", env);
        next = unsafe { next.offset(1) };
    }
}
```


## String and str

``` rust
String
&str
std::ffi::CString
std::ffi::CStr
std::ffi::OsString
std::ffi::OsStr
std::path::Path
std::path::PathBuf
&[u8]
&[u8;N]
Vec<u8>
&u8
&' static str
```
copy from [Rust中的String,&str和str都是什么？](https://zhuanlan.zhihu.com/p/384496181)

## as_bytes() method

``` rust
b"world" == "world".as_bytes()
[1,2,3i32].to_vec()
```

## split

``` rust
use std::str::FromStr;

#[derive(Debug)]
struct Person {
    name: String,
    age: usize,
}

impl FromStr for Person {
    type Err = String;
    fn from_str(s: &str) -> Result<Person, Self::Err> {
        if s.len() == 0 {
            Err("invalid length".to_string())
        } else {
            let v: Vec<&str> = s.split(',').collect();
            let name = v[0];
            if name.len() == 0 {
                return Err("no name found".to_string());
            }
            let age = v[1].parse::<usize>();
            if let Ok(age) = age {
                return Ok(Person {
                    name: String::from(name),
                    age: age,
                });
            }

            Err("invalid age".to_string())
        }
    }
}
```

## splitn

``` rust
impl FromStr for Person {
    type Err = String;
    fn from_str(s: &str) -> Result<Person, Self::Err> {
        if s.len() == 0 {
            Err("invalid length".to_string())
        } else {
            let mut split = s.splitn(2, ',');
            let name = split.next().unwrap();
            if name.len() == 0 {
                return Err("name not found".to_string());
            }
            if let Some(age_str) = split.next() {
                if let Ok(age) = age_str.parse() {
                    return Ok(Person {
                        name: name.to_string(),
                        age,
                    });
                }
            }
            Err("name not found".to_string())
        }
    }
}
```

## reading reference
[Rust 中的生命周期 —— 从 StrSplit 实例说开去](https://zhuanlan.zhihu.com/p/442574694)

```
str —— [T]，表示为一串字符序列（a sequence of characters），编译期无法确定其长度（dynamically sized）；
&str —— &[T]，表示为一个胖指针（fat pointer），ptr 指向切片首地址、length 表示切片长度，编译期可以确定其长度为 16 字节；
String —— Vec<T>，表示为一个胖指针（fat pointer），ptr 指向字符串堆内存的首地址、length 表示字符串当前长度、capacity 表示分配的堆内存的总容量。堆内存支持动态扩展和收缩。编译期可以确定其长度为 24 字节。
```
