erlang中的link/0函数可以使2个进程互相联系在一起，其中一个进程结束后，另外一个会收到这个进程的结束原因，我们可以根据这个原因信息来进行一些处理。

但要注意的是，link/0函数如果使用不当的话，在一个进程结束后，另外一个进程会紧跟着结束。下面直接上代码。
```
-module(linker).

%% API
-export([start/0,
	 start/1,
	 start2/1
	]).

%% 这里直接生成一个进程
start() ->
	spawn(fun() ->
		  receive
			  T ->
						  %% 输出任何收到的信息
			  io:format("T:~p， I~n", [T])
		  end
	  end).

%% 这里对进程进行link()函数操作
start(Pid) ->
	spawn(fun() ->
		  link(Pid),
		  receive
			  T ->
			  io:format("T:~p~n", [T])
		  end
	  end).

%% 跟上面start/1函数一样，多加了
%% process_flag(trap_exit, true) 操作
start2(Pid) ->
	spawn(fun() ->
		  process_flag(trap_exit, true),
		  link(Pid),
		  receive
			  T ->
			 io:format("T:~p, I am ~p ~n", [T, self()])
		  end
	  end).
```
上面建立了一个名为linker.erl的文件，下面我们在erlang shell里面进行操作：
```
$erlc linker.erl
$erl
Erlang/OTP 19 [erts-8.1] [source] [64-bit] [smp:3:3] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V8.1  (abort with ^G)
1> Pid = linker:start().
<0.60.0>
2> Pid2 = linker:start(Pid).
<0.62.0>
3> processes().
[<0.0.0>,<0.1.0>,<0.4.0>,<0.30.0>,<0.31.0>,<0.33.0>,
 <0.34.0>,<0.35.0>,<0.36.0>,<0.38.0>,<0.39.0>,<0.40.0>,
 <0.41.0>,<0.42.0>,<0.44.0>,<0.45.0>,<0.46.0>,<0.47.0>,
 <0.48.0>,<0.49.0>,<0.50.0>,<0.51.0>,<0.52.0>,<0.53.0>,
 <0.54.0>,<0.58.0>,<0.60.0>,<0.62.0>]
4>
4> exit(Pid, kill_process).
true
5> processes().
[<0.0.0>,<0.1.0>,<0.4.0>,<0.30.0>,<0.31.0>,<0.33.0>,
 <0.34.0>,<0.35.0>,<0.36.0>,<0.38.0>,<0.39.0>,<0.40.0>,
 <0.41.0>,<0.42.0>,<0.44.0>,<0.45.0>,<0.46.0>,<0.47.0>,
 <0.48.0>,<0.49.0>,<0.50.0>,<0.51.0>,<0.52.0>,<0.53.0>,
 <0.54.0>,<0.58.0>]
6>

```
可以看到上面生成了2个进程，进行link()函数后，2个进程链接在一起。后来对Pid进行exit/2函数操作，2个进程没有任何信息就同时结束了。
下面我们继续探讨一下：
```
6> Pid3 = linker:start().
<0.67.0>
7> Pid4 = linker:start2(Pid3).
<0.69.0>
8> processes().
[<0.0.0>,<0.1.0>,<0.4.0>,<0.30.0>,<0.31.0>,<0.33.0>,
 <0.34.0>,<0.35.0>,<0.36.0>,<0.38.0>,<0.39.0>,<0.40.0>,
 <0.41.0>,<0.42.0>,<0.44.0>,<0.45.0>,<0.46.0>,<0.47.0>,
 <0.48.0>,<0.49.0>,<0.50.0>,<0.51.0>,<0.52.0>,<0.53.0>,
 <0.54.0>,<0.58.0>,<0.67.0>,<0.69.0>]
9> exit(Pid3, kill_process).
T:{'EXIT',<0.60.0>,kill_process}, I am <0.69.0>
true
10>processes().
[<0.0.0>,<0.1.0>,<0.4.0>,<0.30.0>,<0.31.0>,<0.33.0>,
 <0.34.0>,<0.35.0>,<0.36.0>,<0.38.0>,<0.39.0>,<0.40.0>,
 <0.41.0>,<0.42.0>,<0.44.0>,<0.45.0>,<0.46.0>,<0.47.0>,
 <0.48.0>,<0.49.0>,<0.50.0>,<0.51.0>,<0.52.0>,<0.53.0>,
 <0.54.0>,<0.58.0>]
```
可以看到多使用了process_flag(trap_exit, true)的函数后，不会跟着结束的进程无信息结束掉。
结论：
单独使用link/0函数，2个链接的进程会同时结束；而多使用了process_flag(trap_exit, true)的进程则不会。
这个在<<Designing for Scalability with Erlang/OTP>>书中有提到：
>Remember, though, that links are bidirectional, so if the server dies for some reason while
client and server are linked, this will by default kill the client too, which you may not
want to happen. If that’s the case, use a monitor instead of a link, as we explain in
“Monitors”.
Exit signals can be trapped by calling the process_flag(trap_exit, true) function. This
converts exit signals into messages of the form {'EXIT', Pid, Reason}, where Pid is the
process identifier of the process that has died and Reason is the reason it has terminated.
These messages are stored in the recipient’s mailbox and processed in the same way as all
other messages. When a process is trapping exits, the exit signal is not propagated to any
of the processes in its link set.Why does a process exit? This can happen for two reasons. If a process has no more code
to execute, it terminates normally . The Reason propagated will be the atom normal. Abnormal
termination is initiated in case of a runtime error, receiving an exit signal when not
trapping exits, or by calling the exit BIFs. Called with a single argument, exit(Reason)
will terminate the calling process with reason Reason, which will be propagated in the exit
signal to any other processes to which the exiting one is linked. When the exit BIF is called
with two arguments, exit(Pid, Reason), it sends an exit signal with reason Reason to the
process Pid. This will have the same effect as if the calling process had terminated with
reason Reason.
