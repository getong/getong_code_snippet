# rust trait

## PartialOrd , PartialEq

```
比较运算符实际上也是某些 trait 的语法糖，不过比较运算符所实现的 trait 只有2个：std::cmp::PartialEq和 std::cmp::PartialOrd。

其中，==和!= 实现的是 PartialEq，<、>、>= 和 <=实现的是 PartialOrd。

标准库中，std::cmp 这个 mod 下有4个 trait，而且直观来看 Ord 和 Eq 岂不是更好？但 Rust 对于这4个 trait 的处理是很明确的。因为在浮点数有一个特殊的值叫 NaN，这个值表示未定义的一个浮点数。在 Rust 中可以用0.0f32 / 0.0f32来求得其值，这个数是一个都确定的值，但它表示的是一个不确定的数，那么NaN != NaN 的结果是啥？标准库告诉我们是 true。但这么写有不符合Eq定义里的total equal（每位一样两个数就一样）的定义。因此有了 PartialEq这么一个定义，NaN 这个情况就给它特指了。

为了普适的情况，Rust 的编译器就选择了PartialOrd 和PartialEq来作为其默认的比较符号的trait。
```
