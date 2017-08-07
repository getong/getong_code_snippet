# compile git from source

``` shell
$ wget -c https://www.kernel.org/pub/software/scm/git/git-2.14.0.tar.xz
$ wget -c https://www.kernel.org/pub/software/scm/git/git-2.14.0.tar.sign
$ gpg --verify git-2.14.0.tar.sign git-2.14.0.tar.xz
$ tar xaf git-2.14.0.tar.xz
$ cd git-2.14.0
$ make configure
$ ./configure --prefix=/usr/local/git-2.14.0
$ make -j`nproc` all doc info pdf
$ make -j`nproc` test
$ sudo make install install-doc install-html install-info install-pdf
```
