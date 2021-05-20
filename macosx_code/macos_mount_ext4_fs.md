# macos mount ext4 file system

``` shell
brew install --cask macfuse
brew install ext4fuse
diskutil list
mkdir mount_dir
sudo ext4fuse -o allow_other /dev/disk2s1 mount_dir
```
