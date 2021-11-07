# rust AsRef and AsMut

## trait definition

在Rust中实现上述两个trait， 则表明能够将一个类型转换为另一个类型的引用。
``` rust
trait AsRef<T: ?Sized> {
    fn as_ref(&self) -> &T;
}
trait AsMut<T: ?Sized> {
    fn as_mut(&mut self) -> &mut T;
}
```
copy from [Rust笔记 - AsRef and AsMut trait](Rust笔记 - AsRef and AsMut trait)


std::convert 下面，还有另外两个 Trait，AsRef/AsMut，它们功能是配合泛型，在执行引用操作的时候，进行自动类型转换。

## AsRef
AsRef 提供了一个方法 .as_ref()。

对于一个类型为 T 的对象 foo，如果 T 实现了 AsRef<U>，那么，foo 可执行 .as_ref() 操作，即 foo.as_ref()。操作的结果，我们得到了一个类型为 &U 的新引用。

注：

与 Into<T> 不同的是，AsRef<T> 只是类型转换，foo 对象本身没有被消耗；
T: AsRef<U> 中的 T，可以接受 资源拥有者（owned）类型，共享引用（shared referrence）类型 ，可变引用（mutable referrence）类型。


## AsMut
AsMut<T> 提供了一个方法 .as_mut()。它是 AsRef<T> 的可变（mutable）引用版本。

对于一个类型为 T 的对象 foo，如果 T 实现了 AsMut<U>，那么，foo 可执行 .as_mut() 操作，即 foo.as_mut()。操作的结果，我们得到了一个类型为 &mut U 的可变（mutable）引用。

注：在转换的过程中，foo 会被可变（mutable）借用。

copy from [AsRef 和 AsMut](https://wiki.jikexueyuan.com/project/rust-primer/intoborrow/asref.html)
