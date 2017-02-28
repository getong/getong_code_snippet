# compile emacs from source

```
apt-get build-dep emacs
apt-get install -y libwebkitgtk-3.0-dev autoconf automake libtool texinfo build-essential xorg-dev libgtk2.0-dev libjpeg-dev libncurses5-dev libdbus-1-dev libgif-dev libtiff5-dev libm17n-dev libpng12-dev librsvg2-dev libotf-dev libgnutls28-dev libxml2-dev stow libx11-dev xaw3dg-dev libxft-dev libmagickcore-dev libmagick++-dev libgpm-dev libghc-gconf-dev libm17n-dev

wget -c https://github.com/emacs-mirror/emacs/archive/emacs-25.2-rc2.tar.gz
tar xzf emacs-25.2-rc2.tar.gz
cd emacs-25.2-rc2
./autogen.sh
./configure --prefix=/usr/local/emacs-25.1.91 --with-xwidgets
make -j4 # 这里的cpu是4核心的
make docs
make check
sudo make install
sudo make install-doc
```
