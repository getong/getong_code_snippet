# rust reference

## structs containning references
Whenever a reference type appears inside another type's definition, you must write out its lifetime. You can write this:

``` rust
struct S {
	r: &'static i32
}

struct S<'a> {
	r: &'a i32
}
```
