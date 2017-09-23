# rust types

## basic types
```
str 不可变、静态的字符串
String 可变、不定长的字符串
```

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
