# compile emacs on debian

``` shell
sudo apt-get install -y stow build-essential libx11-dev xaw3dg-dev libjpeg-dev libpng12-dev libgif-dev libtiff5-dev libncurses5-dev libxft-dev librsvg2-dev libmagickcore-dev libmagick++-dev libxml2-dev libgpm-dev libghc-gconf-dev libotf-dev libm17n-dev libgnutls28-dev libgtk-3-dev libwebkitgtk-dev libwebkitgtk-3.0-dev

wget -c https://github.com/emacs-mirror/emacs/archive/emacs-25.2.tar.gz
tar xzf emacs-25.2.tar.gz
cd emacs-25.2
./autogen.sh
./configure --prefix=/usr/local/emacs-25.2 --with-xwidgets
make -j`nproc`
make -j`nproc` docs
make -j`nproc` check
sudo make install
sudo make install-doc
```
