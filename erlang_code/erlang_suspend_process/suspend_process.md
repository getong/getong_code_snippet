# erlang suspend process

```
$erl
Erlang/OTP 19 [erts-8.2.2] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V8.2.2  (abort with ^G)
1> Pid = suspend:start().
<0.60.0>
2> process_info(Pid).
[{current_function,{suspend,start_fun,0}},
 {initial_call,{erlang,apply,2}},
 {status,waiting},
 {message_queue_len,0},
 {messages,[]},
 {links,[]},
 {dictionary,[]},
 {trap_exit,false},
 {error_handler,error_handler},
 {priority,normal},
 {group_leader,<0.51.0>},
 {total_heap_size,233},
 {heap_size,233},
 {stack_size,1},
 {reductions,3},
 {garbage_collection,[{max_heap_size,#{error_logger => true,
                                       kill => true,
                                       size => 0}},
                      {min_bin_vheap_size,46422},
                      {min_heap_size,233},
                      {fullsweep_after,65535},
                      {minor_gcs,0}]},
 {suspending,[]}]
3> self().
<0.58.0>
4> erlang:suspend_process(Pid).
true
5> process_info(Pid).
[{current_function,{suspend,start_fun,0}},
 {initial_call,{erlang,apply,2}},
 {status,suspended},
 {message_queue_len,0},
 {messages,[]},
 {links,[]},
 {dictionary,[]},
 {trap_exit,false},
 {error_handler,error_handler},
 {priority,normal},
 {group_leader,<0.51.0>},
 {total_heap_size,233},
 {heap_size,233},
 {stack_size,1},
 {reductions,3},
 {garbage_collection,[{max_heap_size,#{error_logger => true,
                                       kill => true,
                                       size => 0}},
                      {min_bin_vheap_size,46422},
                      {min_heap_size,233},
                      {fullsweep_after,65535},
                      {minor_gcs,0}]},
 {suspending,[]}]
6> process_info(self()).
[{current_function,{erl_eval,do_apply,6}},
 {initial_call,{erlang,apply,2}},
 {status,running},
 {message_queue_len,0},
 {messages,[]},
 {links,[<0.52.0>]},
 {dictionary,[]},
 {trap_exit,false},
 {error_handler,error_handler},
 {priority,normal},
 {group_leader,<0.51.0>},
 {total_heap_size,4185},
 {heap_size,4185},
 {stack_size,24},
 {reductions,8371},
 {garbage_collection,[{max_heap_size,#{error_logger => true,
                                       kill => true,
                                       size => 0}},
                      {min_bin_vheap_size,46422},
                      {min_heap_size,233},
                      {fullsweep_after,65535},
                      {minor_gcs,0}]},
 {suspending,[{<0.60.0>,1,0}]}]
7> Pid ! abc.
abc
8>
8>
8> process_info(Pid).
[{current_function,{suspend,start_fun,0}},
 {initial_call,{erlang,apply,2}},
 {status,suspended},
 {message_queue_len,1},
 {messages,[abc]},
 {links,[]},
 {dictionary,[]},
 {trap_exit,false},
 {error_handler,error_handler},
 {priority,normal},
 {group_leader,<0.51.0>},
 {total_heap_size,233},
 {heap_size,233},
 {stack_size,1},
 {reductions,3},
 {garbage_collection,[{max_heap_size,#{error_logger => true,
                                       kill => true,
                                       size => 0}},
                      {min_bin_vheap_size,46422},
                      {min_heap_size,233},
                      {fullsweep_after,65535},
                      {minor_gcs,0}]},
 {suspending,[]}]
9> erlang:re
read_timer/1      read_timer/2      ref_to_list/1     register/2
registered/0      resume_process/1
9> erlang:resume_process(Pid).
Msg:abc
true
10> process_info(Pid).
[{current_function,{suspend,start_fun,0}},
 {initial_call,{erlang,apply,2}},
 {status,waiting},
 {message_queue_len,0},
 {messages,[]},
 {links,[]},
 {dictionary,[]},
 {trap_exit,false},
 {error_handler,error_handler},
 {priority,normal},
 {group_leader,<0.51.0>},
 {total_heap_size,233},
 {heap_size,233},
 {stack_size,1},
 {reductions,17},
 {garbage_collection,[{max_heap_size,#{error_logger => true,
                                       kill => true,
                                       size => 0}},
                      {min_bin_vheap_size,46422},
                      {min_heap_size,233},
                      {fullsweep_after,65535},
                      {minor_gcs,0}]},
 {suspending,[]}]
11> process_info(self()).
[{current_function,{erl_eval,do_apply,6}},
 {initial_call,{erlang,apply,2}},
 {status,running},
 {message_queue_len,0},
 {messages,[]},
 {links,[<0.52.0>]},
 {dictionary,[]},
 {trap_exit,false},
 {error_handler,error_handler},
 {priority,normal},
 {group_leader,<0.51.0>},
 {total_heap_size,9358},
 {heap_size,2586},
 {stack_size,24},
 {reductions,18190},
 {garbage_collection,[{max_heap_size,#{error_logger => true,
                                       kill => true,
                                       size => 0}},
                      {min_bin_vheap_size,46422},
                      {min_heap_size,233},
                      {fullsweep_after,65535},
                      {minor_gcs,7}]},
 {suspending,[]}]

```
