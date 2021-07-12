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


## yew architecture

```
To understand how this example works, you first need to understand
the Elm architecture26, which influences Yew. The Elm architecture
consists of three core concepts:
• Model: The state of the application.
• View: A way to turn the state into the UI (HTML).
• Update: A way to update the state based on the
message (Msg) triggered by user interaction on the UI.
```
see example: [practical-rust-web-projects](https://github.com/Apress/practical-rust-web-projects)

## Component trait

``` rust
/// Components are the basic building blocks of the UI in a Yew app. Each Component
/// chooses how to display itself using received props and self-managed state.
/// Components can be dynamic and interactive by declaring messages that are
/// triggered and handled asynchronously. This async update mechanism is inspired by
/// Elm and the actor model used in the Actix framework.
pub trait Component: Sized + 'static {
    /// Messages are used to make Components dynamic and interactive. Simple
    /// Component's can declare their Message type to be `()`. Complex Component's
    /// commonly use an enum to declare multiple Message types.
    type Message: 'static;

    /// Properties are the inputs to a Component and should not mutated within a
    /// Component. They are passed to a Component using a JSX-style syntax.
    /// ```
    ///# use yew::{Html, Component, Properties, ComponentLink, html};
    ///# struct Model;
    ///# #[derive(Clone, Properties)]
    ///# struct Props {
    ///#     prop: String,
    ///# }
    ///# impl Component for Model {
    ///#     type Message = ();type Properties = Props;
    ///#     fn create(props: Self::Properties,link: ComponentLink<Self>) -> Self {unimplemented!()}
    ///#     fn update(&mut self,msg: Self::Message) -> bool {unimplemented!()}
    ///#     fn change(&mut self, _: Self::Properties) -> bool {unimplemented!()}
    ///#     fn view(&self) -> Html {
    /// html! {
    ///     <Model prop="value" />
    /// }
    ///# }}
    /// ```
    type Properties: Properties;

    /// Components are created with their properties as well as a `ComponentLink` which
    /// can be used to send messages and create callbacks for triggering updates.
    fn create(props: Self::Properties, link: ComponentLink<Self>) -> Self;

    /// Components handle messages in their `update` method and commonly use this method
    /// to update their state and (optionally) re-render themselves.
    fn update(&mut self, msg: Self::Message) -> ShouldRender;

    /// When the parent of a Component is re-rendered, it will either be re-created or
    /// receive new properties in the `change` lifecycle method. Component's can choose
    /// to re-render if the new properties are different than the previously
    /// received properties. Most Component's will use props with a `PartialEq`
    /// impl and will be implemented like this:
    /// ```
    ///# use yew::{Html, Component, ComponentLink, html, ShouldRender};
    ///# struct Model{props: ()};
    ///# impl Component for Model {
    ///#     type Message = ();type Properties = ();
    ///#     fn create(props: Self::Properties,link: ComponentLink<Self>) -> Self {unimplemented!()}
    ///#     fn update(&mut self,msg: Self::Message) -> bool {unimplemented!()}
    ///#     fn view(&self) -> Html {unimplemented!()}
    /// fn change(&mut self, props: Self::Properties) -> ShouldRender {
    ///     if self.props != props {
    ///         self.props = props;
    ///         true
    ///     } else {
    ///         false
    ///     }
    /// }
    ///# }
    /// ```
    /// Components which don't have properties should always return false.
    fn change(&mut self, _props: Self::Properties) -> ShouldRender;

    /// Components define their visual layout using a JSX-style syntax through the use of the
    /// `html!` procedural macro. The full guide to using the macro can be found in [Yew's
    /// documentation](https://yew.rs/concepts/html).
    fn view(&self) -> Html;

    /// The `rendered` method is called after each time a Component is rendered but
    /// before the browser updates the page.
    /// ## Examples
    /// ```rust
    ///# use yew::{Html, Component, ComponentLink, html, ShouldRender};
    ///# struct Model{props: ()};
    ///# impl Model { fn setup_element(&self) { } }
    ///# impl Component for Model {
    ///#     type Message = ();type Properties = ();
    ///#     fn create(props: Self::Properties,link: ComponentLink<Self>) -> Self {unimplemented!()}
    ///#     fn update(&mut self,msg: Self::Message) -> bool {unimplemented!()}
    ///#     fn view(&self) -> Html {unimplemented!()}
    ///#     fn change(&mut self, _props: Self::Properties) -> ShouldRender { unimplemented!() }
    /// fn rendered(&mut self, first_render: bool) {
    ///    if first_render {
    ///      self.setup_element(); // Similar to 'mounted' in other frameworks
    ///    }
    /// }
    ///# }
    /// ```
    fn rendered(&mut self, _first_render: bool) {}

    /// The `destroy` method is called right before a Component is unmounted.
    fn destroy(&mut self) {}
}
```
