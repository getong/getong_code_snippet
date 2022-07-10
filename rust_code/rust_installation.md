# rust installation

## install using ustc

``` shell
$ export RUSTUP_DIST_SERVER="https://rsproxy.cn"
$ export RUSTUP_UPDATE_ROOT="https://rsproxy.cn/rustup"
$ export CARGO_HOME=/backup/rust_installation/cargo
$ export RUSTUP_HOME=/backup/rust_installation/rustup
$ curl --proto '=https' --tlsv1.2 -sSf https://rsproxy.cn/rustup-init.sh | sh
$ source $CARGO_HOME/env
$ export RUST_SRC_PATH="$(rustc --print sysroot)/lib/rustlib/src/rust/library"
$ rustup toolchain add nightly
$ rustup component add rls-preview rust-analysis rust-src

$ rustup toolchain install nightly
$ rustup component add rustc-dev --toolchain=nightly
$ cargo install tokio-console
$ cargo install savage
$ cargo install cargo-expand
$ cargo install sea-orm-cli

$ git clone https://github.com/rust-lang/rust-analyzer
$ cd rust-analyzer
$ git checkout tags/2022-06-27 -b 2022-06-27
$ cargo xtask install --server

// copy from https://github.com/rust-lang/rustfmt/issues/4454
## ensure rustfmt
echo 'edition = "2021"\n' >> ~/.rustfmt.toml
```

The `cargo/env` file is just like this:

``` shell
#!/bin/sh
# rustup shell setup
# affix colons on either side of $PATH to simplify matching
case ":${PATH}:" in
    *:"/backup/rust_installation/cargo/bin":*)
        ;;
    *)
        # Prepending path in case a system-installed rustc needs to be overridden
        export PATH="/backup/rust_installation/cargo/bin:$PATH"
        ;;
esac

```

## abscissa

``` shell
cargo install abscissa

abscissa new my_app

cd my_app
cargo build
```

## cargo config file

``` shell
emacs $CARGO_HOME/config

---------------
[source.crates-io]
replace-with = 'rsproxy'

[source.rsproxy]
registry = "https://rsproxy.cn/crates.io-index"

[registries.rsproxy]
index = "https://rsproxy.cn/crates.io-index"

[net]
git-fetch-with-cli = true
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
rustup override set nightly
rustup override set stable
rustup component add rls --toolchain nightly
rustup component add rust-analysis --toolchain nightly
rustup component add rust-src --toolchain nightly
rustup component add rust-docs
## set toolchain ngithly
rustup default nightly
rustup docs
rustup show
```

## cargo usage

``` shell
cargo install cargo-generate
cargo new project_name --bin
cargo build --release
cargo run
cargo run --quiet
cargo update
cargo bench
cargo test
carge install
cargo check
cargo clean
# cargo install clippy
rustup component add clippy-preview
cargo install cargo-edit cargo-outdated
cargo install cargo-web
cargo install wasm-bindgen-cli wasm-pack
cargo install cargo-fuzz

cd project_name
cargo add async-std
## tokio library
cargo add tokio
## command option
cargo add clap
## time operation library
cargo add chrono

cargo add tokio@1.9.0 --features full
cargo add mio --features "os-poll os-ext net"
cargo add tokio_stream --features "sync"

cargo clippy
// get the build detail report
cargo +nightly build -Z timings
```

## rust src path

``` shell

```

## rustdoc and cargo

``` shell
$ rustdoc main.rs
$ cargo doc
```

## switch stable and nightly

``` shell
rustup override set nightly
rustup show

rustup override set stable
rustup show
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

or use ustc

``` shell
[source.crates-io]
replace-with = 'ustc'

[source.ustc]
registry = "git://mirrors.ustc.edu.cn/crates.io-index"
```
copy from [Rust Crates 源使用帮助](https://mirrors.ustc.edu.cn/help/crates.io-index.html)

## install evcxr

``` shell
rustup component add rust-src
cargo install evcxr_repl
evcxr
```
see [Evcxr REPL](https://github.com/google/evcxr/blob/master/evcxr_repl/README.md)
see [Implement a Rust REPL #1120](https://github.com/rust-lang/rust/issues/1120)

## install rustlings

``` shell
cd
curl -L https://git.io/rustlings | bash
# Or if you want it to be installed to a different path:
curl -L https://git.io/rustlings | bash -s mypath/

cd rustlings
rustlings watch
```

## log, env_logger, openssl

``` shell
cargo add log env_logger openssl
```

## update installed crates

``` shell
cargo install cargo-update
cargo install-update -a
cargo install-update crate1 crate2 ...
## self update
cargo-update
```

## wasi

``` shell
cargo install cargo-wasi
curl https://wasmtime.dev/install.sh -sSf | bash

cargo wasi build
cargo wasi build --release
cargo wasi run
cargo wasi test
cargo wasi bench
```

## install wasm-pack error in debian with openssl not found

``` shell
sudo apt install libssl-dev pkg-config
```

## offline mode

``` shell
cargo build --offline
```

## rustdoc

``` shell
rustup doc --reference
rustup doc --std
```

## rust-script

``` shell
cargo install rust-script
```

## cargo doc

The doc will be generated in the target/doc.
The project doc homepage will be target/doc/{project_name}/index.html .
``` shell
cargo doc

cargo doc --open
```

## cargo-edit 0.8.0 bug in linux
the bug info:

```
cargo upgrade
    Updating 'https://github.com/rust-lang/crates.io-index' index
Command failed due to unhandled error: invalid version 0 on git_proxy_options; class=Invalid (3)
```
solved by:

``` shell
cargo install cargo-edit --features "vendored-libgit2"
```

copy from [cargo upgrade gives "unhandled error: invalid version 0 on git_proxy_options"](https://github.com/killercup/cargo-edit/issues/510)

## rust-toolchain
under the root of the root crate project, a file named `rust-toolchain` use the rust version.
Like this:

``` shell
nightly
```
or:

``` shell
nightly-2021-09-24
```

## install specific nightly version

``` shell
rustup toolchain install nightly-2021-09-24
# or
rustup install nightly-2021-09-24
```
copy from [Change nightly Rust version?](https://stackoverflow.com/questions/67024062/change-nightly-rust-version)
[Is it possible to download previous nightly builds?](https://stackoverflow.com/questions/27758387/is-it-possible-to-download-previous-nightly-builds)

install component:

``` shell
rustup toolchain install nightly-2021-09-24 --component rust-src
rustup toolchain install nightly --component rust-docs
```
copy from [Components](https://rust-lang.github.io/rustup/concepts/components.html)


## expand the code macro

``` shell
cd project_root_dir
cargo expand
```

## install rust-analyzer

``` shell
git clone https://github.com/rust-analyzer/rust-analyzer
cd rust-analyzer
git checkout tags/2022-03-21 -b 2022-03-21
cargo xtask install --server
```
or install with rustup

``` shell
rustup component add rust-analyzer-preview
```
the install path is `~/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu/bin/rust-analyzer`

copy from [分享一种安装/更新/切换rust-analyzer版本的方法](https://rustcc.cn/article?id=52912db2-85cb-4da8-afeb-020ae41871dd)
