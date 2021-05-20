# macos mount ext4 file system

``` shell
brew install --cask macfuse
brew install ext4fuse
diskutil list
mkdir mount_dir
sudo ext4fuse -o allow_other /dev/disk2s1 mount_dir
```

copy from [Apple苹果MacOSX系统下使用ext4fuse读取Linux Ext4格式硬盘的文件|Catalina|Mojave|High Sierra](https://www.liujason.com/article/788.html)
