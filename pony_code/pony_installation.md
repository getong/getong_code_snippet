# pony installation

## install ponyup

``` shell
wget -c https://raw.githubusercontent.com/ponylang/ponyup/latest-release/ponyup-init.sh
sh ponyup-init.sh

echo 'export PATH=/home/gerald/.local/share/ponyup/bin:$PATH' >> ~/.zshrc
source ~/.zshrc
```

## Install Pony

``` shell
ponyup update ponyc nightly

ponyup update ponyc release
```
Not work.
