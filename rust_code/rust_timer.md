# rust timer

## timeout

``` rust
tokio::time::timeout(tokio::time::Duration::new(5,0),async move{
            loop{
                let mut sock_add = 10000;
                let sock = tokio::net::UdpSocket::bind(format!("0.0.0.0:{}",sock_add)).await;
                match sock{
                    Ok(a)=>{
                        let r = Arc::new(a);
                        let s = r.clone();
                        tokio::spawn(async move {
                            loop{
                                let mut buf = [0u8;1024];
                                let b = r.recv_from(&mut buf).await;
                                match b{
                                    Ok(a)=>{
                                        println!("{:?} bytes received from:{:?}", &buf[0..a.0],a.1);
                                    },
                                    Err(e)=>{println!("{}",e)}
                                }
                            }
                        });
                        tokio::spawn(async move {
                            let mut msgbuild = dns::DnsMessageBuilder::new(dns::DnsMessageType::Query);
                            let msg = msgbuild.question(dns::DnsQuestion::new("_services._dns-sd._udp.local.".to_string(), false)).build();
                            let msg = msg.serialize();
                            let len = s.send_to(&msg, "224.0.0.251:5353").await;
                            match len{
                                Ok(a)=>{
                                    println!("bytes sent:{}",a);
                                },
                                Err(e)=>{println!("{}",e)}
                            };
                        });
                    },
                    Err(e)=>{
                        if sock_add<65500{
                            sock_add+=1;
                        }else{
                            println!("could not send mDNS: {}",e);
                            break
                        }
                    }
                }
            };
        }).await.unwrap();
```

copy from https://users.rust-lang.org/t/timeout-to-cancel-loop/70484/9

## sleep

``` rust
use tokio::time::{self, Duration, Instant};

#[tokio::main]
async fn main() {
    let sleep = time::sleep(Duration::from_millis(10));
    tokio::pin!(sleep);

    loop {
        tokio::select! {
            () = &mut sleep => {
                println!("timer elapsed");
                sleep.as_mut().reset(Instant::now() + Duration::from_millis(50));
            },
        }
    }
}
```
copy from https://rcos.io/static/internal_docs/tokio/time/struct.Sleep.html

##

``` rust
use futures::{stream, StreamExt}; // 0.3.13
use std::time::{Duration, Instant};
use tokio::time; // 1.3.0

#[tokio::main]
async fn main() {
    let interval = time::interval(Duration::from_millis(10));

    let forever = stream::unfold(interval, |mut interval| async {
        interval.tick().await;
        do_something().await;
        Some(((), interval))
    });

    let _now = Instant::now();
    forever.for_each(|_| async {}).await;
}

async fn do_something() {
    eprintln!("do_something");
}
```
copy from https://stackoverflow.com/questions/66863385/how-can-i-use-tokio-to-trigger-a-function-every-period-or-interval-in-seconds
