# rust ffi

## type from std::os::raw

``` rust
type_alias! { "schar.md", c_schar = i8, NonZero_c_schar = NonZeroI8; }
type_alias! { "uchar.md", c_uchar = u8, NonZero_c_uchar = NonZeroU8; }
type_alias! { "short.md", c_short = i16, NonZero_c_short = NonZeroI16; }
type_alias! { "ushort.md", c_ushort = u16, NonZero_c_ushort = NonZeroU16; }
type_alias! { "int.md", c_int = i32, NonZero_c_int = NonZeroI32; }
type_alias! { "uint.md", c_uint = u32, NonZero_c_uint = NonZeroU32; }
type_alias! { "long.md", c_long = i32, NonZero_c_long = NonZeroI32;
#[cfg(any(target_pointer_width = "32", windows))] }
type_alias! { "ulong.md", c_ulong = u32, NonZero_c_ulong = NonZeroU32;
#[cfg(any(target_pointer_width = "32", windows))] }
type_alias! { "long.md", c_long = i64, NonZero_c_long = NonZeroI64;
#[cfg(all(target_pointer_width = "64", not(windows)))] }
type_alias! { "ulong.md", c_ulong = u64, NonZero_c_ulong = NonZeroU64;
#[cfg(all(target_pointer_width = "64", not(windows)))] }
type_alias! { "longlong.md", c_longlong = i64, NonZero_c_longlong = NonZeroI64; }
type_alias! { "ulonglong.md", c_ulonglong = u64, NonZero_c_ulonglong = NonZeroU64; }
type_alias_no_nz! { "float.md", c_float = f32; }
type_alias_no_nz! { "double.md", c_double = f64; }
```
