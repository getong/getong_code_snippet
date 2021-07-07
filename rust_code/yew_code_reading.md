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

## scheduler

``` rust
thread_local! {
    /// This is a global scheduler suitable to schedule and run any tasks.
    ///
    /// Exclusivity of mutable access is controlled by only accessing it through a set of public
    /// functions.
    static SCHEDULER: RefCell<Scheduler> = Default::default();
}

/// A routine which could be run.
pub(crate) trait Runnable {
    /// Runs a routine with a context instance.
    fn run(self: Box<Self>);
}

/// This is a global scheduler suitable to schedule and run any tasks.
#[derive(Default)]
struct Scheduler {
    // Main queue
    main: VecDeque<Box<dyn Runnable>>,

    // Component queues
    destroy: VecDeque<Box<dyn Runnable>>,
    create: VecDeque<Box<dyn Runnable>>,
    update: VecDeque<Box<dyn Runnable>>,
    render: VecDeque<Box<dyn Runnable>>,

    // Stack
    rendered: Vec<Box<dyn Runnable>>,
}

impl Scheduler {
    /// Pop next Runnable to be executed according to Runnable type execution priority
    fn next_runnable(&mut self) -> Option<Box<dyn Runnable>> {
        self.destroy
            .pop_front()
            .or_else(|| self.create.pop_front())
            .or_else(|| self.update.pop_front())
            .or_else(|| self.render.pop_front())
            .or_else(|| self.rendered.pop())
            .or_else(|| self.main.pop_front())
    }
}
```
The `Scheduler` is run by sequence, order by `destroy`, `create`, `update`, `render`, `rendered`, `main` property.
The element inside the `Scheduler` is `Runnable`, and the trait has its method, `run`.

The `SCHEDULER` variable is thread-local, it might just because the web is single thread.

The append the `Runnalble` element:

``` rust
/// Execute closure with a mutable reference to the scheduler
#[inline]
fn with(f: impl FnOnce(&mut Scheduler)) {
    SCHEDULER.with(|s| f(&mut *s.borrow_mut()));
}

/// Push a generic Runnable to be executed
#[inline]
pub(crate) fn push(runnable: Box<dyn Runnable>) {
    with(|s| s.main.push_back(runnable));
}

```
Just get the `SCHEDULER` variable and `push_back` the element to the queue.
