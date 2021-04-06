# rust webassembly

## add web_sys crate

``` shell
cargo add web_sys
```


## install webassembly environment

``` shell
rustup target list | grep wasm

rustup target add wasm32-wasi
rustup target add wasm32-unknown-emscripten
rustup target add wasm32-unknown-unknown

curl https://get.wasmer.io -sSfL | sh

## wasi environment
cargo new hello_example
cd hello_example
cargo build --target=wasm32-wasi
wasmer run target/wasm32-wasi/debug/hello_example.wasm
```
