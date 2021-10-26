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
