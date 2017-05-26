# erlang write file

## write file function:

``` erlang
{messages, List} = erlang:process_info(Pid, messages).
file:write_file("message1.txt", lists:foldl(fun(E, A) -> [io_lib:fwrite("~w~n",[E])|A]  end, io_lib:fwrite("~w~n", [hd(List)]), tl(List)), [append]).
```

Change the `~p` to `~w`, write one term in one line.
The `~p` will write one term in many lines but pretty prints.

## get the first few nubers of a line

``` shell
sed  's/\(.\{20\}\).*/\1/' msg.txt
```

Change the `20` to be the number of characters of you wanted.
