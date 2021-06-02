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

## install wasm-pack

```
curl https://rustwasm.github.io/wasm-pack/installer/init.sh -sSf |  sh

## new a wasm pack project
wasm cargo generate --git https://github.com/rustwasm/wasm-pack-template

## change directory
cd $project

## build wasm
wasm-pack build
wasm-pack build --target web --out-name wasm --out-dir ./static
```

## book
[Rust and WebAssembly](https://rustwasm.github.io/docs/book/)
