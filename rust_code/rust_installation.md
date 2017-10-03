# rust installation

## install using ustc

``` shell
$ export RUSTUP_DIST_SERVER=https://mirrors.ustc.edu.cn/rust-static
$ export RUSTUP_UPDATE_ROOT=https://mirrors.ustc.edu.cn/rust-static/rustup
$ wget https://sh.rustup.rs -O rustup-init.sh
$ sh rustup-init.sh
$ echo 'export PATH=$HOME/.cargo/bin:$PATH' >> ~/.zshrc
$ source $HOME/.cargo/env
```

## rustup usage

``` shell
## update the rustup script
rustup self update

## delete updateup script
rustup self uninstall

## update the rust
rustup update

## component
rustup component add rust-src
rustup component add rust-analysis

```

## nightly build

``` shell
rustup self update
rustup update nightly
rustup component add rls --toolchain nightly
rustup component add rust-analysis --toolchain nightly
rustup component add rust-src --toolchain nightly
```

## cargo usage

``` shell
cargo install rustfmt
cargo install racer
cargo new project_name --bin
cargo build --release
cargo run
cargo update
carge install
```

## rust src path

``` shell
export RUST_SRC_PATH="$(rustc --print sysroot)/lib/rustlib/src/rust/src"
```
