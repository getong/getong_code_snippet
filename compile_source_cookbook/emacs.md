# compile emacs
## On Debian
``` shell
sudo apt-get install -y stow build-essential libx11-dev xaw3dg-dev libjpeg-dev libpng12-dev libgif-dev libtiff5-dev libncurses5-dev libxft-dev librsvg2-dev libmagickcore-dev libmagick++-dev libxml2-dev libgpm-dev libghc-gconf-dev libotf-dev libm17n-dev libgnutls28-dev libgtk-3-dev libwebkitgtk-dev libwebkitgtk-3.0-dev texinfo texlive texlive-metapost

wget -c https://github.com/emacs-mirror/emacs/archive/emacs-25.2.tar.gz
tar xzf emacs-25.2.tar.gz
cd emacs-emacs-25.2
./autogen.sh
./configure --prefix=/usr/local/emacs-25.2 --with-xwidgets
make -j`nproc`
make -j`nproc` check
make -j`nproc` docs
sudo make install
sudo make install-doc
```

## On CentOS
compile methods almost alike, just install the deps:

``` shell
yum install -y `yum deplist emacs | grep provider | awk -F: '{print $2}' | awk '{print $1}' | xargs`
yum install -y libX11-devel libjpeg-turbo-devel libpng-devel libtiff-devel libXpm-devel giflib-devel openjpeg-devel gtk2-devel ncurses-devel m17n-lib-devel texinfo
texlive texinfo-tex texlive-dvips texlive-metapost libXpm-devel openjpeg2-devel turbojpeg-devel gnutls-devel libxml2-devel GConf2-devel dbus-devel wxGTK-devel gtk3-devel webkitgtk3-devel webkitgtk-devel
```
