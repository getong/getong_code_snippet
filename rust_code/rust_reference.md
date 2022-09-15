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
## coercion
强制隐式转换，是Rust中仅有的类型隐式转换。

## cow

``` rust
use std::borrow::Cow;

let mut lst = Cow::from(&[1,2,3]);
```


## ref and & operator, match

``` rust

fn main() {
    let x = &false;
    print_type_name_of(x);

    let &x = &false;
    print_type_name_of(x);

    let ref x = &false;
    print_type_name_of(x);
}

fn print_type_name_of<T>(_: T) {
    println!("{}", unsafe { std::any::type_name::<T>() })
}

```
output :

``` rust
&bool
bool
&&bool
```

`match` and `ref` usage:

``` rust
enum Favour<'a> {
    Nor(u32),
    NorRef(u32),
    Ref(&'a u32),
    RefRef(&'a u32),
}

fn config(data: &u32) {
    println!("log data: {}", data);
}

// fn con(data: ref u32){  //expected type, found keyword `ref`
//     println!("log data: {}", data);
// }

fn log(fav: Favour) {
    match fav {
        Favour::Nor(data) => {
            config(&data);
            print_type_name_of(data);
        },
        Favour::NorRef(ref data) => {
            config(data);
            print_type_name_of(data);
        },
        Favour::Ref(data) => {
            config(data);
            print_type_name_of(data);
        },
        Favour::RefRef(ref data) => {
            config(data);
            print_type_name_of(data);
        }
    }
}

fn print_type_name_of<T>(_: T) {
    println!("{}", unsafe { std::any::type_name::<T>() })
}

fn main() {
    log(Favour::Nor(1));
    log(Favour::Ref(&2));
    log(Favour::NorRef(3));
    log(Favour::RefRef(&4));
}


log data: 1
u32
log data: 2
&u32
log data: 3
&u32
log data: 4
&&u32
```

copy from [理解 Rust 引用和借用](https://zhuanlan.zhihu.com/p/59998584)


## for reference

``` rust
for (i, item) in bytes.iter().enumerate() {
    if *item == b' ' {
        return i;
    }
}

for (i, &item) in bytes.iter().enumerate() {
    if item == b' ' {
        return i;
    }
}

for (i, item) in bytes.iter().enumerate() {
    if item == &b' ' {
        return i;
    }
}
```
The three styles code are the same.


## subtype

```
F<T> is covariant over T if T being a subtype of U implies that F<T> is a subtype of F<U> (subtyping "passes through")
F<T> is contravariant over T if T being a subtype of U implies that F<U> is a subtype of F<T>
F<T> is invariant over T otherwise (no subtyping relation can be derived)
Variance of types is automatically determined as follows

Type	               Variance in 'a	    Variance in T
&'a T	               covariant    	    covariant
&'a mut T	           covariant	        invariant
*const T		                            covariant
*mut T		                                invariant
[T] and [T; n]		                        covariant
fn() -> T		                            covariant
fn(T) -> ()		                            contravariant
std::cell::UnsafeCell<T>	                invariant
std::marker::PhantomData<T>	                covariant
dyn Trait<T> + 'a      covariant	        invariant
```
copy from [Variance](https://doc.rust-lang.org/reference/subtyping.html#variance)

## mutable reference does not implement Copy trait

```
  --> src/main.rs:10:20

8       let mystr = input;
            ----- move occurs because `mystr` has type `&mut String`, which does not implement the `Copy` trait
9       let _otherstr = mystr;
                        ----- value moved here
10      println!("{}", mystr);
                       ^^^^^ value borrowed here after move

```

## reading reference
[Rust 中的迭代器](https://zhuanlan.zhihu.com/p/458806498)

```
一个类型（Type）实现 Deref / DerefMut trait，编译器会在三种情况下执行解引用：

1、*x —— 显式解引用，根据 x 所在的上下文（mutable contexts / immutable contexts），等价于执行 *(std::ops::Deref::deref(&x)) / *(std::ops::DerefMut::deref_mut(&mut x))，* 解引用操作符；

2、x.method(call-params) —— 方法调用时执行隐式解引用，可能调用的候选方法包括：

associated methods on specific traits
statically dispatching to a method if the exact self-type of the left-hand-side is known
dynamically dispatching if the left-hand-side expression is an indirect trait object
因此，查找方法名时需获取得到所有的候选类型（a list of candidate receiver types） —— 通过对 x 执行多次解引用获取得到所有的候选类型。

3、类型转换（Type coercions），一个简单的例子，代码 2，type-coercions，


Rust 编译器执行 Deref coercion 时会区分可变和不可变，The Book - How Deref Coercion Interacts with Mutability：

From &T to &U when T: Deref<Target=U>
From &mut T to &mut U when T: DerefMut<Target=U>
From &mut T to &U when T: Deref<Target=U> 一个可变借用（可变借用是排他的，只能有一个）可以解引用为不可变借用，满足 Rust 的借用规则；反过来不行，将一个不可变借用（不可变借用可以有多个）解引用为可变借用会破坏 Rust 的借用规则。
```

## doc reading
[Rust's as_ref vs as_deref](https://www.fpcomplete.com/blog/rust-asref-asderef/)

## ref mut variable example

``` rust
/// Keeps track of a behavior.
#[derive(Clone, Debug, serde::Deserialize, serde::Serialize, PartialEq)]
pub enum State<A> {
    /// Executes an action.
    ActionState(A),
    /// Converts `Success` into `Failure` and vice versa.
    InvertState(Box<State<A>>),
    /// Ignores failures and always return `Success`.
    AlwaysSucceedState(Box<State<A>>),
    /// Keeps track of waiting for a period of time before continuing.
    ///
    /// f64: Total time in seconds to wait
    ///
    /// f64: Time elapsed in seconds
    WaitState(f64, f64),
    /// Waits forever.
    WaitForeverState,
    /// Keeps track of an `If` behavior.
    /// If status is `Running`, then it evaluates the condition.
    /// If status is `Success`, then it evaluates the success behavior.
    /// If status is `Failure`, then it evaluates the failure behavior.
    IfState(Box<Behavior<A>>, Box<Behavior<A>>, Status, Box<State<A>>),
    /// Keeps track of a `Select` behavior.
    SelectState(Vec<Behavior<A>>, usize, Box<State<A>>),
    /// Keeps track of an `Sequence` behavior.
    SequenceState(Vec<Behavior<A>>, usize, Box<State<A>>),
    /// Keeps track of a `While` behavior.
    WhileState(Box<State<A>>, Vec<Behavior<A>>, usize, Box<State<A>>),
    /// Keeps track of a `WhenAll` behavior.
    WhenAllState(Vec<Option<State<A>>>),
    /// Keeps track of a `WhenAny` behavior.
    WhenAnyState(Vec<Option<State<A>>>),
    /// Keeps track of an `After` behavior.
    AfterState(usize, Vec<State<A>>),
}


impl<A: Clone> State<A> {
pub fn tick<E, F>(&mut self, e: &E, f: &mut F) -> (Status, f64)
    where
        E: UpdateEvent,
        F: FnMut(ActionArgs<E, A>) -> (Status, f64),
        A: Debug,
    {
    match (upd, self) {
         (_, &mut WhileState(ref mut ev_cursor, ref rep, ref mut i, ref mut cursor)) => {
         **cursor = State::new(rep[*i].clone());
         }
    }
}
}
```
copy from [bonsai](https://github.com/Sollimann/bonsai)

*cursor is Box::new(State), **cursor is State.

cursor's data type is &mut Box::new(State).
