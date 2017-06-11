# process_proxy

## riak_core use `process_proxy.erl` to proxy the msg

process_proxy.erl register a local name, when receive a msg, it send it to the `parent_pid`.
