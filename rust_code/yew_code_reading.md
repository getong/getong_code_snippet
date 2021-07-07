# yew code reading

## Callback

``` rust
/// Universal callback wrapper.
/// <aside class="warning">
/// Use callbacks carefully, because if you call one from the `update` loop
/// of a `Component` (even from JS) it will delay a message until next.
/// Callbacks should be used from JS callbacks or `setTimeout` calls.
/// </aside>
/// An `Rc` wrapper is used to make it cloneable.
pub enum Callback<IN> {
    /// A callback which can be called multiple times
    Callback(Rc<dyn Fn(IN)>),
    /// A callback which can only be called once. The callback will panic if it is
    /// called more than once.
    CallbackOnce(Rc<CallbackOnce<IN>>),
}

type CallbackOnce<IN> = RefCell<Option<Box<dyn FnOnce(IN)>>>;

impl<IN> Callback<IN> {
    /// This method calls the callback's function.
    pub fn emit(&self, value: IN) {
        match self {
            Callback::Callback(cb) => cb(value),
            Callback::CallbackOnce(rc) => {
                let cb = rc.replace(None);
                let f = cb.expect("callback in CallbackOnce has already been used");
                f(value)
            }
        };
    }

    /// Creates a callback from an `FnOnce`. The programmer is responsible for ensuring
    /// that the callback is only called once. If it is called more than once, the callback
    /// will panic.
    pub fn once<F>(func: F) -> Self
    where
        F: FnOnce(IN) + 'static,
    {
        Callback::CallbackOnce(Rc::new(RefCell::new(Some(Box::new(func)))))
    }

    /// Creates a "no-op" callback which can be used when it is not suitable to use an
    /// `Option<Callback>`.
    pub fn noop() -> Self {
        Self::from(|_| {})
    }
}

```
The `once` is based on the `RefCell` pointer, and first check the `func` is called before by calling:

``` rust
let cb = rc.replace(None);
let f = cb.expect("callback in CallbackOnce has already been used");
f(value)
```
The `RefCell::replace` method is defined as, and the return result will be the called function before.

``` rust
    /// Replaces the wrapped value with a new one, returning the old value,
    /// without deinitializing either one.
    ///
    /// This function corresponds to [`std::mem::replace`](../mem/fn.replace.html).
    ///
    /// # Panics
    ///
    /// Panics if the value is currently borrowed.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::cell::RefCell;
    /// let cell = RefCell::new(5);
    /// let old_value = cell.replace(6);
    /// assert_eq!(old_value, 5);
    /// assert_eq!(cell, RefCell::new(6));
    /// ```
    #[inline]
    #[stable(feature = "refcell_replace", since = "1.24.0")]
    #[track_caller]
    pub fn replace(&self, t: T) -> T {
        mem::replace(&mut *self.borrow_mut(), t)
    }
```
