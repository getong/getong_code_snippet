# rust box keyword

```
impl<T> Box<T> {
    /// Allocates memory on the heap and then moves `x` into it.
    /// [...]
    #[stable(feature = "rust1", since = "1.0.0")]
    #[inline(always)]
    pub fn new(x: T) -> Box<T> {
        box x
    }
}


NOTE: This reply is a bit old. Since it talks about internals and unstable features, things have changed a little bit. The basic mechanism remains the same though, so the answer is still capable of explaining the undelying mechanisms of box.

What does box x usually uses to allocate and free memory?

The answer is the functions marked with lang items exchange_malloc for allocation and exchange_free for freeing. You can see the implementation of those in the default standard library at heap.rs#L112 and heap.rs#L125.

In the end the box x syntax depends on the following lang items:

owned_box on a Box struct to encapsulate the allocated pointer. This struct does not need a Drop implementation, it is implemented automatically by the compiler.
exchange_malloc to allocate the memory.
exchange_free to free the previously allocated memory.
```
copy from [What does the box keyword do?](https://stackoverflow.com/questions/30352802/what-does-the-box-keyword-do)

## Box::leak() function
a variable with static lifetime with runtime size.

``` rust
let boxed = Box::new(String::from("Crab"));
let raw_ptr = unsafe { Box::into_raw(boxed) };
let _ = unsafe { Box::from_raw(raw_ptr) }; // will be freed
```

``` rust

// Excerpt from the standard library documentation
use std::alloc::{dealloc, Layout};
use std::ptr;

let x = Box::new(String::from("Hello"));
let p = Box::into_raw(x);
unsafe {
    ptr::drop_in_place(p);
    dealloc(p as *mut u8, Layout::new::<String>());
}
```
copy from [Memory management](https://anssi-fr.github.io/rust-guide/05_memory.html)
