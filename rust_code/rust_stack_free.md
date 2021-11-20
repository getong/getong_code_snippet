# rust stack free

## code:

``` rust
fn triangular(n: u64) -> u64 {
    if n == 0 {
        0
    } else {
        n + triangular(n - 1)
    }
}


fn triangular_safe(n: u64) -> u64 {
    trampoline(|n| move |_| {
        if n == 0 {
            0
        } else {
            n + yield (n - 1)
        }
    })(n)
}

fn trampoline<Arg, Res, Gen>(
    f: impl Fn(Arg) -> Gen
) -> impl Fn(Arg) -> Res
where
    Res: Default,
    Gen: Generator<Res, Yield = Arg, Return = Res> + Unpin,
{
    move |arg: Arg| {
        let mut stack = Vec::new();
        let mut current = f(arg);
        let mut res = Res::default();

        loop {
            match Pin::new(&mut current).resume(res) {
                GeneratorState::Yielded(arg) => {
                    stack.push(current);
                    current = f(arg);
                    res = Res::default();
                }
                GeneratorState::Complete(real_res) => {
                    match stack.pop() {
                        None => return real_res,
                        Some(top) => {
                            current = top;
                            res = real_res;
                        }
                    }
                }
            }
        }
    }
}
```
It uses the [Generators](https://doc.rust-lang.org/beta/unstable-book/language-features/generators.html)
copy from [Stack-safety for free?](https://hurryabit.github.io/blog/stack-safety-for-free/)
