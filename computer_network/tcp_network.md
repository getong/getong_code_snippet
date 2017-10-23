# tcp network

## 200ms
see [200ms latency between TCP send and tcpdump only with large messages](https://unix.stackexchange.com/questions/31158/200ms-latency-between-tcp-send-and-tcpdump-only-with-large-messages)
> You're passing too few bytes in each call to send or write. You need to try to pass at least 2KB per call, or better, 4KB per call. If possible, accumulate the entire logical message and send it at once. This will save system calls, pack your packets more efficiently, and prevent delayed ACK from destroying your latency.


## sync and async network framework
sync network framework: http, rpc
async network framework: amqp, mqtt
