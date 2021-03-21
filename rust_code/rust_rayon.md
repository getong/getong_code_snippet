# rust rayon

.iter(), .iter_mut(), into_iter()在rayon中是这样写的:
.par_iter(), .par_iter_mut(), par_into_iter(). 所以你只要加上par_，你的代码就会变得快很多。(par的意思是 "并行")

其他方法也一样:.chars()就是.par_chars()，以此类推。

copy from [外部crate](https://kumakichi.github.io/easy_rust_chs/Chapter_59.html)
