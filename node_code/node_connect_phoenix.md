# node connect to phoenix

copy from [Use phoenix.js in other project](https://elixirforum.com/t/use-phoenix-js-in-other-project/23466)

## start the phoenix

``` shell
iex -S mix phx.server
```

## node connect to the phoenix

``` shell
mkdir temp
cd temp
yarn add phoenix-channels
node
> const { Socket } = require('phoenix-channels')
> let socket = new Socket("ws://127.0.0.1:4000/socket")
> socket.connect()
> let channel = socket.channel("room:lobby", {})
> channel.join().receive("ok", resp => { console.log("Joined successfully", resp) }).receive("error", resp => { console.log("Unable to join", resp) })
```
