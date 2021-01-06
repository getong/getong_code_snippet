# rust types

## primitive types
```
array
bool
char
numeric types
slice
str 不可变、静态的字符串
String 可变、不定长的字符串
tuple
function
pointer
```

## numeric types

```
i8 u8
i16 u16
i32 u32
i64 i64
i128 u128 ## nightly-only
isize usize
f32 f64
```
see [primitive type](https://doc.rust-lang.org/std/primitive.array.html)
see [Primitive Types](https://doc.rust-lang.org/book/first-edition/primitive-types.html)


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


## rust 里的 & 不是指针，是borrow, 对应的 &mut 是mutable borrow.

## pointer
use with as and unsafe

``` rust
let x = 5;
let raw = &x as *const i32;

let points_at = unsafe { *raw };

println!("raw points at {}", points_at);
```

## Three pointer types: references, boxes and unsafe poointers.

``` rust
// References
&T
&mut T

// Boxes
let t = 12;
let b = Box::new(t);

// Raw Pointers
*mut T
*const T
```

## ?Sized, Sized, Unsize
?Sized includes Sized and Unsize

## test Copy trait

``` rust
fn test_copy<T: Copy>(i: T) {
    println!("copy trait")
}

fn main() {
    let a = "String".to_string();
    test_copy(a);
}
```

## clone trait

``` rust
struct MyStruct;
impl Copy for MyStruct { }
impl Clone for MyStruct {
fn clone(&self) -> Mystruct {
    *self
    }
}
```
or

``` rust
#derive(Copy, Clone)]
struct MyStruct;
```

## Clone, Copy trait

```
常见的数字类型、bool类型、共享借用指针&，都是具有 Copy 属性的类型。而 Box、Vec、可写借用指针&mut 等类型都是不具备 Copy 属性的类型。

对于数组类型，如果它内部的元素类型是Copy，那么这个数组也是Copy类型。

对于tuple类型，如果它的每一个元素都是Copy类型，那么这个tuple会自动实现Copy trait。

对于struct和enum类型，不会自动实现Copy trait。而且只有当struct和enum内部每个元素都是Copy类型的时候，编译器才允许我们针对此类型实现Copy trait。

Clone 的全名是 std::clone::Clone。它的完整声明是这样的：

pub trait Clone : Sized {
    fn clone(&self) -> Self;
    fn clone_from(&mut self, source: &Self) {
        *self = source.clone()
    }
}
它有两个关联方法，其中 clone_from 是有默认实现的，它依赖于 clone 方法的实现。clone 方法没有默认实现，需要我们手动实现。

clone 方法一般用于“基于语义的复制”操作。所以，它做什么事情，跟具体类型的作用息息相关。比如对于 Box 类型，clone 就是执行的“深拷贝”，而对于 Rc 类型，clone 做的事情就是把引用计数值加1。

虽然说，Rust中 clone 方法一般是用来执行复制操作的，但是你如果在自定义的 clone 函数中做点什么别的工作编译器也没法禁止，你可以根据情况在 clone 函数中编写任意的逻辑。但是有一条规则需要注意：对于实现了 Copy 的类型，它的 clone 方法应该跟 Copy 语义相容，等同于按位拷贝。

Copy 和 Clone 两者的区别和联系有：

Copy内部没有方法，Clone内部有两个方法。
Copy trait 是给编译器用的，告诉编译器这个类型默认采用 copy 语义，而不是 move 语义。Clone trait 是给程序员用的，我们必须手动调用clone方法，它才能发挥作用。
Copy trait不是你想实现就实现，它对类型是有要求的，有些类型就不可能 impl Copy。Clone trait 没有什么前提条件，任何类型都可以实现（unsized 类型除外）。
Copy trait规定了这个类型在执行变量绑定、函数参数传递、函数返回等场景下的操作方式。即这个类型在这种场景下，必然执行的是“简单内存拷贝”操作，这是由编译器保证的，程序员无法控制。Clone trait 里面的 clone 方法究竟会执行什么操作，则是取决于程序员自己写的逻辑。一般情况下，clone 方法应该执行一个“深拷贝”操作，但这不是强制的，如果你愿意，也可以在里é¢启动一个人工智能程序，都是有可能的。
如果你确实需要Clone trait执行“深拷贝”操作，编译器帮我们提供了一个工具，我们可以在一个类型上添加#[derive(Clone)]，来让编译器帮我们自动生成那些重复的代码。
然而Rust语言规定了当T: Copy的情况下，Clone trait代表的含义。即：当某变量let t: T;，符合T: Copy时， 它调用 let x = t.clone() 方法的时候，它的含义必须等同于“简单内存拷贝”。也就是说，clone的行为必须等同于let x = std::ptr::read(&t);，也等同于let x = t;。当T: Copy时，我们不要在Clone trait里面乱写自己的逻辑。所以，当我们需要指定一个类型是 Copy 的时候，最好顺便也指定它 Clone 的行为，就是编译器为我们自动生成的那个逻辑。正因为如此，在希望让一个类型具有 Copy 性质的时候，一般使用 #[derive(Copy, Clone)] 这种方式，这种情况下它们俩最好一起出现，避免手工实现 Clone 导致错误。
```
copy from [Clone VS Copy](https://zhuanlan.zhihu.com/p/21730929)


## dyn trait

``` rust
// A reference to a trait object is a fat pointer: (data_ptr, vtable_ptr)
trait Test {
    fn add(&self) -> i32;
    fn sub(&self) -> i32;
    fn mul(&self) -> i32;
}

// This will represent our home brewn fat pointer to a trait object
   #[repr(C)]
struct FatPointer<'a> {
    /// A reference is a pointer to an instantiated `Data` instance
    data: &'a mut Data,
    /// Since we need to pass in literal values like length and alignment it's
    /// easiest for us to convert pointers to usize-integers instead of the other way around.
    vtable: *const usize,
}

// This is the data in our trait object. It's just two numbers we want to operate on.
struct Data {
    a: i32,
    b: i32,
}

// ====== function definitions ======
fn add(s: &Data) -> i32 {
    s.a + s.b
}
fn sub(s: &Data) -> i32 {
    s.a - s.b
}
fn mul(s: &Data) -> i32 {
    s.a * s.b
}

fn main() {
    let mut data = Data {a: 3, b: 2};
    // vtable is like special purpose array of pointer-length types with a fixed
    // format where the three first values has a special meaning like the
    // length of the array is encoded in the array itself as the second value.
    let vtable = vec![
        0,            // pointer to `Drop` (which we're not implementing here)
        6,            // lenght of vtable
        8,            // alignment

        // we need to make sure we add these in the same order as defined in the Trait.
        add as usize, // function pointer - try changing the order of `add`
        sub as usize, // function pointer - and `sub` to see what happens
        mul as usize, // function pointer
    ];

    let fat_pointer = FatPointer { data: &mut data, vtable: vtable.as_ptr()};
    let test = unsafe { std::mem::transmute::<FatPointer, &dyn Test>(fat_pointer) };

    // And voalá, it's now a trait object we can call methods on
    println!("Add: 3 + 2 = {}", test.add());
    println!("Sub: 3 - 2 = {}", test.sub());
    println!("Mul: 3 * 2 = {}", test.mul());
}
```
copy from [200行代码讲透RUST FUTURES](https://stevenbai.top/rust/futures_explained_in_200_lines_of_rust/)

``` rust
fn some_fn(param1: i32, param2: i32) -> Box<dyn View> {
    if param1 > param2 {
        // do something...
        return &Button {};
    } else {
        // do something...
        return &TextView {};
    }
}
```
or

``` rust
trait Foo {
    fn default_impl(&self) {
        println!("correct impl!");
    }
}

impl Foo {
    fn trait_object() {
        println!("trait object impl");
    }
}

struct Bar {}

impl Foo for Bar {}

fn main() {
    let b = Bar{};
    b.default_impl();
    // b.trait_object();
    Foo::trait_object();
}
```
copy from [捋捋 Rust 中的 impl Trait 和 dyn Trait](https://zhuanlan.zhihu.com/p/109990547)



## simple link list example

``` rust
use std::cell::RefCell;
use std::rc::Rc;
use std::rc::Weak;
struct Node {
    next: Option<Rc<RefCell<Node>>>,
    head: Option<Weak<RefCell<Node>>>,
}

fn main() {
    let first = Rc::new(RefCell::new(Node {
        next: None,
        head: None,
    }));
    let second = Rc::new(RefCell::new(Node {
        next: None,
        head: None,
    }));
    let third = Rc::new(RefCell::new(Node {
        next: None,
        head: None,
    }));
    first.borrow_mut().next = Some(second.clone());
    second.borrow_mut().next = Some(third.clone());
    third.borrow_mut().head = Some(Rc::downgrade(&first));
}
```
Rc<T> implements Clone. Every call to base.clone() increments an internal counter.
Every Drop decrements that counter. When the internal counter reaches zero, the
original instance is freed.
Rc<T> does not allow mutation. To permit that, we need to wrap our
wrapper. Rc<RefCell<T>> is a type that can be used to perform interior mutability. An
object that has interior mutability presents an immutable façade, while internal values
are being modified.

## Pin

``` rust
use std::pin::Pin;
use std::marker::PhantomPinned;

 #[derive(Debug)]
struct Test {
    a: String,
    b: *const String,
    _marker: PhantomPinned,
}

impl Test {
    fn new(txt: &str) -> Pin<Box<Self>> {
        let a = String::from(txt);
        let t = Test {
            a,
            b: std::ptr::null(),
            _marker: PhantomPinned,
        };
        let mut boxed = Box::pin(t);
        let self_ptr: *const String = &boxed.as_ref().a;
        unsafe { boxed.as_mut().get_unchecked_mut().b = self_ptr };

        boxed
    }

    fn a<'a>(self: Pin<&'a Self>) -> &'a str {
        &self.get_ref().a
    }

    fn b<'a>(self: Pin<&'a Self>) -> &'a String {
        unsafe { &*(self.b) }
    }
}

pub fn main() {
    let mut test1 = Test::new("test1");
    let mut test2 = Test::new("test2");

    println!("a: {}, b: {}",test1.as_ref().a(), test1.as_ref().b());
    println!("a: {}, b: {}",test2.as_ref().a(), test2.as_ref().b());
}
```
copy from [200行代码讲透RUST FUTURES](https://stevenbai.top/rust/futures_explained_in_200_lines_of_rust/)


## get the variable type

``` rust
fn print_type_of<T>(_: &T) {
    println!("{}", std::any::type_name::<T>())
}

fn main() {
    let s = "Hello";
    let i = 42;

    print_type_of(&s); // &str
    print_type_of(&i); // i32
    print_type_of(&main); // playground::main
    print_type_of(&print_type_of::<i32>); // playground::print_type_of<i32>
    print_type_of(&{ || "Hi!" }); // playground::main::{{closure}}
}
```
copy from [How do I print the type of a variable in Rust?](https://stackoverflow.com/questions/21747136/how-do-i-print-the-type-of-a-variable-in-rust)

## String, &str

``` rust
// &str to String
let a: String = "hello rust".into();
let b: String = "hello rust".to_string();
let c: String = String::from("hello rust");
let d: String = "hello rust".to_owned();


// String to &str
let e = &String::from("hello rust");
let f_tmp = String::from("hello rust");
let f = f_tmp.as_str();

let g = &f_tmp[1..3];
```
copy from [Rust学习笔记 - 测试与字符串](https://hijiangtao.github.io/2019/05/30/Rust-learning-notes-with-Rustlings-3/)
copy from [Rust中的&str和String有什么区别](https://blog.ykyi.net/2019/10/rust%E4%B8%AD%E7%9A%84str%E5%92%8Cstring%E6%9C%89%E4%BB%80%E4%B9%88%E5%8C%BA%E5%88%AB/)
