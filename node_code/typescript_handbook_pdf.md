# typescript handbook pdf

## on debian compile the typescript handbook pdf

``` shell
git clone https://github.com/kjarmicki/typescript-handbook-to-pdf
cd typescript-handbook-to-pdf
export export OPENSSL_CONF=/etc/ssl/
npm start
```
the main problem is `libssl_conf.so: cannot open shared object file: No such file or directory`, solved it by add `export export OPENSSL_CONF=/etc/ssl/`
copy from [genymotion throws libssl_conf.so: cannot open shared object file: No such file or directory](https://stackoverflow.com/questions/53355217/genymotion-throws-libssl-conf-so-cannot-open-shared-object-file-no-such-file-o)

## typescript-handbook
[typescript-handbook](https://zhongsp.gitbook.io/typescript-handbook/)
