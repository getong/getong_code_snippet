# Dealing with Integer Overflow

## Rust handle overflow differently between Debug and Release mode

```
In debug mode, Rust includes checks for integer overflow that will panic at runtime.
In release mode, there is no integer overflow check, so, no panic.
Instead, Rust will perform two’s complement wrapping, for the u8 type, the value 256 will be wrapped into 0, the value 257 will be wrapped into 1, while the value -1 will be wrapped into 255, and so on.
Hence, be mindful that the values you get might not be the ones you expected. Or you can explicitly enable overflow check by adding overflow-checks = true to your Cargo.toml.
```

## Wrapping the values with wrapping_* methods, like wrapping_add:

``` rust
u8::MAX.wrapping_add(2) == 1u8
0u8.wrapping_sub(1) == u8::MAX
```

## Returning a wrapped value, and an overflow indicator with overflowing_* methods:

``` rust
5u8.overflowing_add(1) == (6u8, false)
u8::MAX.overflowing_add(1) == (0u8, true)
```

## Check and return None if there is an overflow with checked_* methods:

``` rust
5u8.checked_add(1) == Some(6u8)
u8::MAX.checked_add(1) == None
```

## Saturate at the maximum or minimum value of the type with saturating_* methods:

``` rust
u8::MAX.saturating_add(5) == u8::MAX
0u8.saturating_sub(5) == 0u8

```

## To reduce boilerplates, Rust provides Wrapping<T> and Saturating<T> types to automatically provide the desired overflow behavior for your types:

``` rust
use std::num::Saturating;

let a = Saturating(u32::MAX);
(a + 1).0 == u32::MAX
```

## When casting between types, wrapping is the default behavior:

``` rust
(257u32 as u8) == 1u8
```

copy from [01.16.2022 - Rust/Dealing with Integer Overflow](https://www.huy.rocks/everyday/01-16-2022-rust-dealing-with-integer-overflow)
