# rust installation

## install using ustc

``` shell
$ export RUSTUP_DIST_SERVER=https://mirrors.ustc.edu.cn/rust-static
$ export RUSTUP_UPDATE_ROOT=https://mirrors.ustc.edu.cn/rust-static/rustup
$ wget https://sh.rustup.rs -O rustup-init.sh
$ sh rustup-init.sh
## or one line
$ curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
$ echo 'export PATH=$HOME/.cargo/bin:$PATH' >> ~/.zshrc
$ source $HOME/.cargo/env

rustup toolchain add nightly
rustup component add rls-preview rust-analysis rust-src
cargo +nightly install racer

```
## rustup install the stable by default

``` shell
curl https://sh.rustup.rs -sSf | sh -s -- -y --default-toolchain stable
```

## another method install the rust

``` shell
RUSTUP_HOME=~/rustup && CARGO_HOME=~/cargo && PATH=~/cargo/bin:$PATH && RUST_VERSION=1.39.0
wget -c https://static.rust-lang.org/rustup/archive/1.20.2/x86_64-unknown-linux-gnu/rustup-init
chmod +x rustup-init
./rustup-init -y --no-modify-path --profile minimal --default-toolchain $RUST_VERSION
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
rustup install stable
rustup install nightly
rustup update nightly
rustup component add rls --toolchain nightly
rustup component add rust-analysis --toolchain nightly
rustup component add rust-src --toolchain nightly
rustup component add rust-docs
rustup docs
```

## cargo usage

``` shell
cargo install rustfmt
cargo install racer
cargo new project_name --bin
cargo build --release
cargo run
cargo update
cargo bench
cargo test
carge install
cargo install clippy
cargo install cargo-edit

cd project_name
cargo add async-std
## tokio library
cargo add tokio
## command option
cargo add clap
## time operation library
cargo add chrono

cargo clippy
```

## rust src path

``` shell
export RUST_SRC_PATH="$(rustc --print sysroot)/lib/rustlib/src/rust/src"
```

## rustdoc and cargo

``` shell
$ rustdoc main.rs
$ cargo doc
```

## set nightly as default

``` shell
rustup default nightly
```

## cargo mirror

``` shell
# ~/.cargo/config
[source.crates-io]
replace-with = 'tuna'

[source.tuna]
registry = "https://mirrors.tuna.tsinghua.edu.cn/git/crates.io-index.git"
```
copy from [Rust crates.io 索引镜像使用帮助](https://mirrors.tuna.tsinghua.edu.cn/help/crates.io-index.git/)


## install evcxr

``` shell
rustup component add rust-src
cargo install evcxr_repl
evcxr
```
see [Evcxr REPL](https://github.com/google/evcxr/blob/master/evcxr_repl/README.md)
see [Implement a Rust REPL #1120](https://github.com/rust-lang/rust/issues/1120)
