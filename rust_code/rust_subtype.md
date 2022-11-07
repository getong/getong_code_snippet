# rust subtype

## doc reference
[Subtyping and Variance](https://doc.rust-lang.org/nomicon/subtyping.html)
[PhantomData](https://doc.rust-lang.org/nomicon/phantom-data.html)
[逆变、协变与子类型，以及Rust](https://zhuanlan.zhihu.com/p/41814387)
[PhantomData 黑魔法](https://iovxw.net/p/phantomdata-magic/)
[Subtyping and Variance (&mut invariant) violate error in rust](https://stackoverflow.com/questions/74266349/subtyping-and-variance-mut-invariant-violate-error-in-rust)


## code explaination

``` rust
fn main() {
    struct Foo<'a, T> {
        a: &'a mut T,
    }

    let p1 = 1;
    let mut p2 = &p1;
    {
        let p3 = 2;
        let mut p4 = &p3;
        let mut f = Foo {
            a: &mut p4,
        };
        f.a = &mut p2;
        println!("{}", f.a);
    }
    println!("{}", p2);
}
```

>>>
The core of the problem is that f has a single fixed type Foo<'a, &'b i32> and by the variance rules for mutable references, &'b i32 is invariant and thus 'b is invariant.

However, f is used with T as two separate lifetimes via p2 and p4. How does the compiler choose? Well it cannot shorten the lifetime used by p2 into that of p4, because then p2 can be modified to reference something of a smaller lifetime and thus p2 can dangle at the last println! (consider what would happen if you added *f.a = &p3; right after assigning f.a to &mut p2). The only option is for the lifetime used by p4 to be widened to match p2.

Since p4 must match the lifetime of p2, the assignment from p3 is now too short, so you get the error you see.

The second example works because the lifetime used by p2 does not extend after the println! in the inner block, so p3 can satisfy that lifetime.

``` rust
fn main() {
    struct Foo<'a, T> {
        a: &'a mut T,
    }

    let p1 = 1;
    let mut p2 = &p1;
    {
        let p3 = 2;
        let mut p4 = &p3;
        let mut f = Foo {
            a: &mut p4,
        };
        f.a = &mut p2;
        println!("{}", f.a);
    }
    // println!("{}", p2);
}
```


## variable invariant might be move out of scope, and free the memory.