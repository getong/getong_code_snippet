# macosx usage

## brew install

```
# brew 程序本身，Homebrew/Linuxbrew 相同
git -C "$(brew --repo)" remote set-url origin https://mirrors.tuna.tsinghua.edu.cn/git/homebrew/brew.git

# 以下针对 mac OS 系统上的 Homebrew
git -C "$(brew --repo homebrew/core)" remote set-url origin https://mirrors.tuna.tsinghua.edu.cn/git/homebrew/homebrew-core.git
git -C "$(brew --repo homebrew/cask)" remote set-url origin https://mirrors.tuna.tsinghua.edu.cn/git/homebrew/homebrew-cask.git
git -C "$(brew --repo homebrew/cask-fonts)" remote set-url origin https://mirrors.tuna.tsinghua.edu.cn/git/homebrew/homebrew-cask-fonts.git
git -C "$(brew --repo homebrew/cask-drivers)" remote set-url origin https://mirrors.tuna.tsinghua.edu.cn/git/homebrew/homebrew-cask-drivers.git

# 以下针对 Linux 系统上的 Linuxbrew
git -C "$(brew --repo homebrew/core)" remote set-url origin https://mirrors.tuna.tsinghua.edu.cn/git/homebrew/linuxbrew-core.git

# 更换后测试工作是否正常
brew update
```
copy from [Homebrew/Linuxbrew 镜像使用帮助](https://mirrors.tuna.tsinghua.edu.cn/help/homebrew/)

## virtualbox

```
Restart your mac in Recovery mode (cmd + R)

Then open a Terminal and enter : spctl kext-consent add VB5E2TV963

Restart your mac.
```
copy from [Can't Reinstall VirtualBox on Mojave](https://forums.virtualbox.org/viewtopic.php?f=8&t=93246)

## add keyboard shortcuts
see [在Mac中设置Ctrl+C/V进行复制/粘贴](https://support.apple.com/zh-cn/HT201236)

## macosx keyboard shortcuts
see [Mac 键盘快捷键](https://support.apple.com/zh-cn/HT201236)

## change keyboard key position by using Karabiner-Elements

~~## install intel-power-gadget~~

``` shell
brew cask install intel-power-gadget
```
copy from [How I fixed my VirtualBox VMs randomly crashing on macOS](https://angristan.xyz/2020/02/crashes-virtualbox-macos-intel-power-gadget/)

## Remove .DS_Store in macOS

``` shell
defaults write com.apple.desktopservices DSDontWriteNetworkStores true
```
copy from [Remove .DS_Store in macOS](https://wp-mix.com/remove-ds_store-in-macos/)

## install julia

``` shell
brew cask install julia
```

## brew download cache directory

``` shell
~/Library/Caches/Homebrew/downloads
```

## docker desktop

``` shell
https://mirrors.aliyun.com/docker-toolbox/mac/docker-for-mac/stable/Docker.dmg
```

## install IDE

``` shell
brew tap homebrew/cask-versions
brew cask install android-studio visual-studio unity-hub epic-games
```

## Simulator

``` shell
open -a Simulator
```

## android-platform-tools

``` shell
brew cask install android-platform-tools

```

## pandoc

``` shell
brew install pandoc
```


## upgrade

``` shell
brew upgrade
brew upgrade --cask
brew upgrade --cask --greedy
 brew outdated --cask --greedy
```

## install pass

``` shell
brew install pass
```

## find the package download filename

``` shell
brew cache -s package-name
```

## delete ABC input method

``` shell
/usr/libexec/PlistBuddy -c "Delete :AppleEnabledInputSources:1" ~/Library/Preferences/com.apple.HIToolbox.plist
```
copy from [Mac 怎么设置搜狗输入法为默认第一位？](https://www.v2ex.com/amp/t/592752)

## install firefox

``` shell
brew cask install firefox --language=zh
```
copy from [language](https://github.com/Homebrew/homebrew-cask/blob/master/doc/cask_language_reference/stanzas/language.md])

## homebrew usage

``` shell
## Edit this formula
brew edit git

## Print this formula
brew cat git
```

## brew cleanup

``` shell
alias brewski='brew update && brew upgrade && brew cleanup; brew doctor'
```
copy from [别忘了定期执行 brew cleanup 哦](https://www.jianshu.com/p/403140306cb6)

## not mount disk

``` shell
$ sudo diskutil list
$ sudo diskutil info --all | grep "Volume UUID"
$ sudo echo "UUID=3B87FF76-C6DA-49BF-B911-61DE2331E9F5 none ext4 noauto 0 0" >> /etc/fstab

```
The `3B87FF76-C6DA-49BF-B911-61DE2331E9F5` is the disk you don't want to mount automatically.
copy from [MacOS下禁止开机自动挂载分区 [/etc/fstab]](https://blog.csdn.net/qq_38202733/article/details/109631753)

or
``` shell
echo "UUID=791E37B4-82A3-37E7-9F15-3C39359126B4   none  hfs  rw,noauto" >> /etc/fstab
```
copy from [How to Disable USB Auto-mount](https://apple.stackexchange.com/questions/120782/how-to-disable-usb-auto-mount)

use with virtualbox:

``` shell
VBoxManage list usbhost
```
copy from [How to get the UUID of a USB device on a Mac?](https://stackoverflow.com/questions/8305419/how-to-get-the-uuid-of-a-usb-device-on-a-mac/37170840)

## access samba
Finder－>Go（前往）->Connete to Server...（连接服务器）(Command+K)
smb://ip/share

copy from [Mac OS X 访问连接SAMBA共享磁盘](https://www.jianshu.com/p/4f785ae6c29c)
also see [Macos 建立Samba服务器](https://blog.csdn.net/qq_38375620/article/details/101699465)

## disable brew auto update

``` shell
echo "export HOMEBREW_NO_INSTALL_UPGRADE=1" >> ~/.zshrc
source ~/.zshrc
```
copy from [HOMEBREW_NO_AUTO_UPDATE=1 environment variable does not work](https://github.com/Homebrew/brew/issues/12114)

## podman

``` shell
brew install podman
podman machine init
podman machine start
export DOCKER_HOST='unix:///Users/gerald/.local/share/containers/podman/machine/podman-machine-default/podman.sock'
podman pull yandex/clickhouse-server:21.3.20.1
podman run -d --name some-clickhouse-server --ulimit nofile=262144:262144 yandex/clickhouse-server:21.3.20.1
```

## Warning: Bottle missing, falling back to the default domain

``` shell
echo 'export HOMEBREW_BOTTLE_DOMAIN=https://mirrors.ustc.edu.cn/homebrew-bottles/bottles' >> ~/.zshrc

source ~/.zshrc
```

## reinstall

``` shell
brew reinstall --cask sogouinput
```
copy from [Homebrew - Error: Not upgrading 1 `installer manual` cask](https://www.reddit.com/r/Windscribe/comments/soe5hn/homebrew_error_not_upgrading_1_installer_manual/)


## 霞鹜文楷

``` shell
brew install  --cask font-lxgw-wenkai
```

## delete .DS_store
``` shell
defaults write com.apple.desktopservices DSDontWriteNetworkStores -bool TRUE
```
copy from [MacOS禁止.DS_store生成](https://www.jianshu.com/p/3f8008fb3985)
