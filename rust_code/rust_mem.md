# rust mem operation

## forget function

``` rust
std::mem::forget<T>(t: T)
```
Takes ownership and “forgets” about the value without running its destructor.
