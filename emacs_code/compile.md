# compile emacs from source

```
apt-get build-dep emacs
apt-get install libwebkitgtk-3.0-dev
wget -c https://github.com/emacs-mirror/emacs/archive/emacs-25.1.91.tar.gz
tar xzf emacs-25.1.91.tar.gz
cd emacs-25.1.91
./autogen.sh
./configure --prefix=/usr/local/emacs-25.1.91 --with-xwidgets
make -j4 # 这里的cpu是4核心的
make docs
make check
sudo make install
sudo make install-doc
```
