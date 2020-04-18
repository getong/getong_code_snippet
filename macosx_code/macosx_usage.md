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
