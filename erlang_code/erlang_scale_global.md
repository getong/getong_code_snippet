# erlang scale global module

## Issue: Connections stay up
Plan: Automatic disconnects

## Issue: Too many connections
Plan: Avoid fully connected network

## Issue: Expensive when new nodes join
Plan: Make node joins cheaper

## Issue: :global chats a lot
Plan: make :global less chatty

It might be released with erlang 20, and use the Kademlia tree to achieve it.

More info:
[Scaling Distributed Erlang](http://www.elixirconf.eu/elixirconf2016/zandra-norman)
[slide](http://s3.amazonaws.com/erlang-conferences-production/media/files/000/000/074/original/Zandra_Norman_ScalingDistributedErlang.pdf?1462891793)
[video](https://youtu.be/usEs3GPnZDg)
