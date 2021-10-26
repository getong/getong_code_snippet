# rust pointer

## pointer address

```
fn main(){
    let a:i32 = 5;
    //&a先转成raw指针，然后再把指针转成usize，这个可以print的
    let addr = &a as *const i32 as usize;
    println!("addr：0x{:X}",addr);

    //为了验证刚才的地址是不是正确的，我们修改这个指针指向的数据
    //pa就是addr对应的raw指针
    let pa = addr as *mut i32;
    //解引用，*pa其实就是&mut a了，给他赋值100
    unsafe{*pa = 100};

    //打印a，可以看到a已经变成100了
    println!("value:{}",a);
}

```
copy from [rust:打印变量地址](https://blog.csdn.net/varding/article/details/48104893)

## pointer info

``` rust
fn main() {
    //let input = 2049;
    let input: u32 = 0x10111213;
    print_bytes(&input);
}

fn print_bytes(input: &u32) {
    let ptr_u32: *const u32 = input;
    let ptr_u8 = ptr_u32 as *const u8;

    unsafe {
        println!("{:x}-{:x}-{:x}-{:x}",
                 *ptr_u8, *ptr_u8.offset(1),
                 *ptr_u8.offset(2),
                 *ptr_u8.offset(3));
    }
}
```
output:

``` rust
13-12-11-10
```

``` rust
use std::mem;

fn main() {
    let a:u32 = 0x10111213;
    let b:[u8; 4];

    b = unsafe{mem::transmute(a)};

    println!("{:x}-{:x}-{:x}-{:x}", b[0], b[1], b[2], b[3]);
}
```


``` rust
use std::mem;

struct Unit {
    _id: u32,
    _num: u32,
}

fn main() {
    println!("{}", mem::size_of::<Unit>());

    let unit = Unit {
        _id: 123,
        _num: 100
    };

    let mem: [u32;2];
    mem = unsafe { mem::transmute(unit)};
    println!("{:}, {:}", mem[0], mem[1]);
}
```
output:

```
8
123, 100
```

``` rust
use std::collections::HashMap;
use std::mem;

fn main() {
    let map = build_hash_map(10);

    let mem: [usize; 5];
    mem = unsafe{ mem::transmute_copy(&map) };
    //println!("{:x}-{:x}-{}-{}-{:x}", mem[0], mem[1], mem[2], mem[3], mem[4]);

    let capacity_mask = mem[2];
    let size = mem[3];
    let slot_size = capacity_mask + 1;
    let hash_start = mem[4] as *mut usize;
    let pair_start = unsafe {hash_start.add(slot_size) as *const (usize, usize)};

    println!("Hash Table:");
    println!("capacity: {}", map.capacity());
    println!("size: {}", size);
    println!("capacity_mask: {}", capacity_mask);
    println!("slot size: {}", slot_size);
    println!("Heap address: {:p}", hash_start);
    unsafe {
        for index in 0..slot_size {
            let hash_ptr = hash_start.add(index);

            let pair_ptr = pair_start.add(index);
            let key_ptr = pair_ptr as *const usize;
            let val_ptr = key_ptr.add(1);

            let hash_val = *hash_ptr;
            if hash_val > 0 {
                // probe displacement
                // use wrapping_sub to avoid subtracting with overflow
                let displacement = index.wrapping_sub(hash_val) & capacity_mask;

                print!("{:3}: ", index);
                print!("{:p}, {:p}, {:p}; ", hash_ptr, key_ptr, val_ptr);
                //print!("{:21}-{}, {}, {}", hash_val, displacement, *key_ptr, *val_ptr);
                print!("{}, {}, {}", displacement, *key_ptr, *val_ptr);
                println!("");
            }
        }
    }
}

fn build_hash_map(size: usize) -> HashMap<usize, usize> {
    let mut map: HashMap<usize, usize> = HashMap::new();

    for key in 0..size {
        map.insert(key, 10 * key);
        map.insert(key, 10 * key);
    }

    map
}
```
output:

``` rust
Hash Table:
capacity: 29
size: 10
capacity_mask: 31
slot size: 32
Heap address: 0x5602cc488b40
  0: 0x5602cc488b40, 0x5602cc488c40, 0x5602cc488c48; 0, 4, 40
  1: 0x5602cc488b48, 0x5602cc488c50, 0x5602cc488c58; 1, 7, 70
  2: 0x5602cc488b50, 0x5602cc488c60, 0x5602cc488c68; 0, 6, 60
  7: 0x5602cc488b78, 0x5602cc488cb0, 0x5602cc488cb8; 0, 1, 10
 14: 0x5602cc488bb0, 0x5602cc488d20, 0x5602cc488d28; 0, 5, 50
 20: 0x5602cc488be0, 0x5602cc488d80, 0x5602cc488d88; 0, 0, 0
 24: 0x5602cc488c00, 0x5602cc488dc0, 0x5602cc488dc8; 0, 9, 90
 25: 0x5602cc488c08, 0x5602cc488dd0, 0x5602cc488dd8; 0, 3, 30
 26: 0x5602cc488c10, 0x5602cc488de0, 0x5602cc488de8; 1, 8, 80
 27: 0x5602cc488c18, 0x5602cc488df0, 0x5602cc488df8; 1, 2, 20
```

```
+----------------+
|                |
+----------------+
| RandomState.k0 |
+----------------+
| RandomState.k1 |
+----------------+
| capacity_mask  |
+----------------+
| size           |
+----------------+
| heap address   +---+
+----------------+   |
|                |   |
|                |   |
+----------------+   |
                     |
                     v
-----------------------------------+-----------------------+------------
                     | hashes      | key->alue pairsrs     |
---------------------+-------------+-----------------------+------------
```

copy from [查看变量在内存中的存储结构 - Rust](https://zhuanlan.zhihu.com/p/102591451)


## more mem example

``` rust
// 64bit架构
fn main() {
    std::mem::size_of::<*mut u8>(); // 8B，裸指针
    std::mem::size_of::<*mut [u8]>(); // 16B，胖指针，还有8B的⻓度
    std::mem::size_of::<*mut [u8; 4]>(); // 8B，裸指针（⻓度就是4）
    std::mem::size_of::<*mut str>(); // 16B，胖指针，额外8B的⻓度
    std::mem::size_of::<*mut dyn Drop>(); // 16B，胖指针，额外8B的vtable指针
}
```

## std::mem:size_of<T>() -> usize

```
Returns the size of a type in bytes.

More specifically, this is the offset in bytes between successive elements in an array with that item type including alignment padding. Thus, for any type T and length n, [T; n] has a size of n * size_of::<T>().

In general, the size of a type is not stable across compilations, but specific types such as primitives are.

The following table gives the size for primitives.

Type	size_of::<Type>()
()	0
bool	1
u8	1
u16	2
u32	4
u64	8
u128	16
i8	1
i16	2
i32	4
i64	8
i128	16
f32	4
f64	8
char	4
Furthermore, usize and isize have the same size.

The types *const T, &T, Box<T>, Option<&T>, and Option<Box<T>> all have the same size. If T is Sized, all of those types have the same size as usize.

The mutability of a pointer does not change its size. As such, &T and &mut T have the same size. Likewise for *const T and *mut T.
```

## pointer type

```
Box
Cell
RefCell
Rc
Arc
RwLock
Mutex
```
copy from [Smart Pointers in Rust: What, why and how?](https://dev.to/rogertorres/smart-pointers-in-rust-what-why-and-how-oma)

## convert integer to pointer

``` rust
pub const fn null<T>() -> *const T {
    0 as *const T
}
```

## smart pointer

```
智能指针是Rust中一种特殊的数据结构。它与普通指针的本质区别在于普通指针是对值的借用，而智能指针通常拥有对数据的所有权。
它与普通数据结构的区别在于智能指针实现了Deref和Drop这两个traits。实现Deref可以使智能指针能够解引用，而实现Drop则使智能指针具有自动析构的能力。
Deref有一个特性是强制隐式转换：如果一个类型T实现了Deref<Target=U>，则该类型T的引用在应用的时候会被自动转换为类型U。
Deref使得智能指针在使用时被自动解引用，像是不存在一样。
DerefMut和Deref类似，只不过它是返回可变引用的。
```
code example

``` rust
use std::rc::Rc;
fn main() {
    let x = Rc::new("hello");
    println!("{:?}", x.chars());
}
```

```
Drop对于智能指针非常重要，它是在智能指针被丢弃时自动执行一些清理工作，这里所说的清理工作并不仅限于释放堆内存，还包括一些释放文件和网络连接等工作。
```

```
Cell<T>和RefCell<T>, 它们本质上不属于智能指针，而是可以提供内部可变性的容器。内部可变性实际上是一种设计模式，它的内部是通过一些unsafe代码来实现的。
```
code example:

``` rust
use std::cell::RefCell;
fn main() {
    let x = RefCell::new(vec![1, 2, 3]);
    println!("{:?}", x.borrow());
    x.borrow_mut().push(5);
    println!("{:?}", x.borrow());
}

```

smart pointer lists:

``` rust
Box
Rc
Arc
String
Vec
Cow

```
copy from [Rust入坑指南：智能指针](https://juejin.cn/post/6844904086718906381)

```
Cow: 写时复制，即 Copy On Write,是指如果需要对某个变量进行复制时，系统并不会直接复制，而是使用相同的内存空间，在只读的时候，读取相同的空间，而如果发生了数据的写入时，才会进行复制操作。原因就是如果变量内存很大时，直接复制会占用很大的时间，而如果复制完后并不发生写入，则会浪费资源,多用于读多写少的操作。而 Rust 中的 Cow 是一个枚举类型，包含 Borrow(T) 及 Owned(T)。使用 Cow::from(xx) 来创建一个 Cow 时，如果 xx 是引用，对 cow 做任何操作都不会影响到 xx，而 xx 是值，则 xx 会被 move,对其操作，其实就是更改 xx.通俗点说，Cow 就是某个数据的克隆体，不过什么时候克隆，需要调用相关方法的时候，才会被决定。
```
copy from [后端 智能指针](https://www.dazhuanlan.com/sail2011/topics/1666072)
code example:

``` rust
use std::borrow::Cow;

fn abs_all(input: &mut Cow<[i32]>) {
    for i in 0..input.len() {
        let v = input[i];
        if v < 0 {
            // Clones into a vector if not already owned.
            input.to_mut()[i] = -v;
        }
    }
}

// No clone occurs because `input` doesn't need to be mutated.
let slice = [0, 1, 2];
let mut input = Cow::from(&slice[..]);
abs_all(&mut input);

// Clone occurs because `input` needs to be mutated.
let slice = [-1, 0, 1];
let mut input = Cow::from(&slice[..]);
abs_all(&mut input);

// No clone occurs because `input` is already owned.
let mut input = Cow::from(vec![-1, 0, 1]);
abs_all(&mut input);
```
copy from [Enum std::borrow::Cow](https://doc.rust-lang.org/std/borrow/enum.Cow.html)
