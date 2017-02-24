#erlang process_info

```
process_info(self()).
[{current_function,{erl_eval,do_apply,6}},
 {initial_call,{erlang,apply,2}},
 {status,running},
 {message_queue_len,0},
 {messages,[]},
 {links,[<0.293.0>]},
 {dictionary,[]},
 {trap_exit,false},
 {error_handler,error_handler},
 {priority,normal},
 {group_leader,<0.292.0>},
 {total_heap_size,752},
 {heap_size,376},
 {stack_size,24},
 {reductions,5771},
 {garbage_collection,[{max_heap_size,#{error_logger => true,
                                       kill => true,
                                       size => 0}},
                      {min_bin_vheap_size,46422},
                      {min_heap_size,233},
                      {fullsweep_after,65535},
                      {minor_gcs,5}]},
 {suspending,[]}]
```

##current_function
当前函数

##initial_call
初始调用函数

##status
当前进程运行状态

##message_queue_len
消息队列长度

##messages
进程收到的消息

##links
链接进程

##dictionary
进程字典

##trap_exit
捕获进程退出标志

##error_handler
错误处理器

##priority
进程优先级

##group_leader
io输出进程

##total_heap_size
总的堆的大小

##heap_size
堆大小

##stack_size
栈大小

##garbage_collection
进程垃圾回收选项

##suspending
进程挂起信息
