# compile emacs from source

```
apt-get build-dep emacs
apt-get install libwebkitgtk-3.0-dev autoconf automake libtool texinfo build-essential xorg-dev libgtk2.0-dev libjpeg-dev libncurses5-dev libdbus-1-dev libgif-dev libtiff5-dev libm17n-dev libpng12-dev librsvg2-dev libotf-dev libgnutls28-dev libxml2-dev
apt-get install -y stow build-essential libx11-dev xaw3dg-dev libjpeg-dev libpng12-dev libgif-dev libtiff5-dev libncurses5-dev libxft-dev librsvg2-dev libmagickcore-dev libmagick++-dev libxml2-dev libgpm-dev libghc-gconf-dev libotf-dev libm17n-dev libgnutls28-dev
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
