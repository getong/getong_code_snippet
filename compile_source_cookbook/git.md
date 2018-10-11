# compile git from source

``` shell
$ sudo apt-get install -y libcurl4-gnutls-dev libexpat1-dev gettext libz-dev libssl-dev asciidoc xmlto docbook2x dblatex asciidoc-dblatex
$ export VERSION=2.19.1
$ wget -c https://www.kernel.org/pub/software/scm/git/git-$VERSION.tar.xz
$ wget -c https://www.kernel.org/pub/software/scm/git/git-$VERSION.tar.sign
$ gpg --verify git-$VERSION.tar.sign git-$VERSION.tar.xz
$ tar xaf git-$VERSION.tar.xz
$ cd git-$VERSION
$ make configure
$ ./configure --prefix=/usr/local/git-$VERSION
$ make -j`nproc` all doc info pdf
$ make -j`nproc` test
$ sudo make install install-doc install-html install-info install-pdf
```
