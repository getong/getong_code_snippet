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
