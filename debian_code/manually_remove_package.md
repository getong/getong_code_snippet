# manually remove package
Sometimes, it is hard to remove package via `apt-get`.
For example, there is a error during removing:

``` shell
(正在读取数据库 ... 系统当前共安装有 157429 个文件和目录。)
正在卸载 python-gtk2-dev (2.24.0-4) ...
dpkg: 处理软件包 python-gtk2-dev (--remove)时出错：
 子进程 已安装 pre-removal 脚本 返回错误状态 1
dpkg：清理时出错:
 子进程 已安装 post-installation 脚本 返回错误状态 1
在处理时有错误发生：
 python-gtk2-dev
E: Sub-process /usr/bin/dpkg returned an error code (1)

```
Remove it manually:

``` shell
mv /var/lib/dpkg/info/PAQUET.* /tmp/
dpkg --remove --force-remove-reinstreq PAQUET
```

Replace the `PAQUET` with the package name you want to remove.

See the reference [Manually remove a broken package on Debian/Ubuntu](http://www.piprime.fr/1480/manually-remove-broken-package-debian-ubuntu/)
