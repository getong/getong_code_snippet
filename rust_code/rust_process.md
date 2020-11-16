# rust process

## process example

```
use std::process::Command;

fn main() {
    let mut p = Command::new("../sub/target/debug/sub.exe").spawn().unwrap();
    p.wait().unwrap();
}


use std::thread::sleep;
use std::time::Duration;
use std::process::Command;

fn main() {
    let mut p = Command::new("../sub/target/debug/sub.exe").spawn().unwrap();
    loop {
        let status = p.try_wait().unwrap();
        match status {
            Some(status) => {
                if status.success() {
                    println!("sub process over");

                } else {
                    println!("sub process error");
                }
                break;
            }

            None => {
                sleep(Duration::from_secs(1));
                continue;
            }
        }
    }
}



use std::env::args;
use std::thread::sleep;
use std::time::Duration;

fn main() {
    println!("Hello, world!");
    let mut args = args();
    let secs = args.nth(1).unwrap().parse::<u64>().unwrap();

    sleep(Duration::from_secs(secs));
    println!("Bye, world!");
}



use std::process::Command;

fn main() {
    let mut p = Command::new("../sub/target/debug/sub.exe").arg("5").spawn().unwrap();
    p.wait().unwrap();
}


use std::io;
use std::env::args;

fn main() {
    println!("Hello, world!");
    let mut args = args();
    let count = args.nth(1).unwrap().parse::<u64>().unwrap();
    let mut index  = 0;
    let stdin = io::stdin();	// 打开标准输入
    while index < count {
        let mut s = String::new();
        stdin.read_line(&mut s).unwrap();	// 	读取标准输入中的一行
        println!("{}", s.trim());	// trim掉最后的\n

        index += 1;
    }
    println!("Bye, world!");
}



use std::process::{Command, Stdio};
use std::io::{BufRead, Write, BufReader};

fn main() {
    // 消息数组
    let msglist = ["msg1", "msg2", "msg3", "msg4", "msg5"];

    // 启动子进程
    let mut p = Command::new("../sub/target/debug/sub.exe")
        .arg(msglist.len().to_string()) // 传递消息的个数
        .stdin(Stdio::piped())  // 将子进程的标准输入重定向到管道
        .stdout(Stdio::piped()) // 将子进程的标准输出重定向到管道
        .spawn()
        .unwrap();

    let p_stdin = p.stdin.as_mut().unwrap();
    let mut p_stdout = BufReader::new(p.stdout.as_mut().unwrap());
    let mut line = String::new();

    // 接收Hello world
    p_stdout.read_line(&mut line).unwrap();
    println!("{}", line);

    // 循化发送消息
    for msg in msglist.iter() {
        // 发送消息
        println!("write to stdid:{}", msg);
        p_stdin.write(msg.as_bytes()).unwrap();
        p_stdin.write("\n".as_bytes()).unwrap();    // 发送\n，子进程的read_line才会响应

        // 接收消息
        line.clear();   // 需要清空，否则会保留上次的结果
        p_stdout.read_line(&mut line).unwrap();
        println!("read from stdout:{}", line);
    }

    // 接收Bye world
    line.clear();
    p_stdout.read_line(&mut line).unwrap();
    println!("{}", line);

    // 等待子进程结束
    p.wait().unwrap();
}
```
copy from [Rust的并发编程（一）多进程并发](https://blog.csdn.net/zhmh326/article/details/108485752)
