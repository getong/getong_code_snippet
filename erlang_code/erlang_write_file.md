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

## send file
tcp send file, very simple, just call the function.
``` erlang
file:sendfile(Filename, Socket)
file:sendfile(RawFile, Socket, Offset, Bytes, Opts)
```
ssl send file, only send some bytes data, must read the file and send the reading result through the socket.

``` erlang
ssl:send(Socket, Data)
```

## check is file

``` erlang
is_file(Name) ->
    case filelib:is_file(Name) of
	true ->
	    not filelib:is_dir(Name);
	false ->
	    false
    end.
```
copy from pp_record.erl

## datasync before close file

``` erlang
_ = file:datasync(FD),
_ = file:close(FD),
copy from lager_file_backend.erl```

## inode

``` erlang
-include_lib("kernel/include/file.hrl").
case file:read_file_info(Name) of
    {ok, FInfo} ->
        Inode = FInfo#file_info.inode,
```
copy from lager_rotator_default.erl

## format_error
There are the same `format_error` function in module `file` and `io`.

``` erlang
%% io:format_error/1
For a specified error returned by any function in this module, this function returns a descriptive string of the error in English. For file errors, function file:format_error (Posix) is to be called.

%% file:format_error/1
Given the error returned by any function in this module, this function returns a descriptive string of the error in English. For file errors, function format_error/1 in module file is called.
```
They are almost the same.

## symlink

``` erlang
-include_lib("kernel/include/file.hrl").
is_symlink(Filename) ->
    {ok, Info} = file:read_link_info(FileName),
    Info#file_info.type == symlink.
```
copy from [Way to check if directory is a symlink via erlang](https://stackoverflow.com/questions/39002380/way-to-check-if-directory-is-a-symlink-via-erlang)
