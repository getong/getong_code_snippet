时间操作，例如前一天、后一天，最好的方案是先把时间换算成unix时间戳， 然后进行计算，最后换算回来日期。

ejabberd 对erlang不同的版本的时间函数进行了封装，确保了使用正确的函数
[p1_utils](https://github.com/processone/p1_utils)
p1_time_compat.erl 这个模块可以找到对应版本的时间方法.
