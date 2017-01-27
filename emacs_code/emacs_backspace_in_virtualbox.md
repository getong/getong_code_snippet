#virtualbox中emacs backspace按键修改为不是ctrl-h
在虚拟机中，emacs下按backspace会弹出帮助信息，但是在linux下的命令行下，backspace功能很正常。我的物理安装的linux中emacs没有这个问题。于是猜测是emacs中的backspace绑定错误 。
然后搜索一下，还真是emacs在linux下绑定有 问题。
参考[linux下解决emacs的backspace键变成了ctrl+h （C+h）键？](http://blog.csdn.net/loveaborn/article/details/9615323)
添加下列指令到init.el
```
(global-set-key "\C-h" 'backward-delete-char-untabify)
(global-set-key "\d" 'delete-char)
```
