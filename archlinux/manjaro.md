# manjaro

## manjaro installation

copy from [linux 发行版 manjaro 安装指南](https://segmentfault.com/a/1190000022575018)
copy from [Manjaro-GNOME的安装与配置](https://zhuanlan.zhihu.com/p/64799118)
```
$ sudo pacman -Syy
$ sudo pacman-mirrors -i -c China -m rank
$ sudo pacman -Syyu

vim /etc/pacman.conf
-------
[archlinuxcn]
SigLevel = Optional TrustedOnly
Server =https://mirrors.ustc.edu.cn/archlinuxcn/$arch

$ sudo pacman -Syy && sudo pacman -S archlinuxcn-keyring

$ sudo pacman -S typora
```
