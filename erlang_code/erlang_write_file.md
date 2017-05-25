# erlang write file

write file function:

``` erlang
{messages, List} = erlang:process_info(Pid, messages).
file:write_file("message1.txt", lists:foldl(fun(E, A) -> [io_lib:fwrite("~p~n",[E])|A]  end, io_lib:fwrite("~p~n", [hd(List)]), tl(List)), [append]).
```
