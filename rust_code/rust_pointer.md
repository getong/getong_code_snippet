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
