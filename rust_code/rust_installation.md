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
