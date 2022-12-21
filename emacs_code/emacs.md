# emacs

## case

``` emacs-lisp
M-l
Convert following word to lower case (downcase-word).
M-u
Convert following word to upper case (upcase-word).
M-c
Capitalize the following word (capitalize-word).
C-x C-l
Convert region to lower case (downcase-region).
C-x C-u
Convert region to upper case (upcase-region).
```
copy from [Case Conversion Commands](https://www.gnu.org/software/emacs/manual/html_node/emacs/Case.html)

## jump to the matched parenthesis(bracket)

```
C-M-n,C-M-p
```
copy from [请问emacs中如何跳转到和光标所在括号配对的括号处？](https://bbs.csdn.net/topics/70029649)

## coding system

``` emacs-lisp
m-x revert-buffer-with-coding-system
utf-8
```
change the coding headline of the file:

```
%% -*- coding: utf-8 -*-
```

add the following into the ~/.emacs.d/init.el

``` emacs-lisp
(set-default-coding-systems 'utf-8)
```
copy from [Working with Coding Systems and Unicode in Emacs](https://www.masteringemacs.org/article/working-coding-systems-unicode-emacs)

## ansi-term mode

>>>
You may want to simply switch between character mode and line mode while using the terminal. C-c C-j will run term-line-mode, which treats the terminal buffer more like a normal text-buffer in which you can move the cursor and yank text. You can switch back to character mode by running term-char-mode with C-c C-k.

copy from [Copy/Paste in emacs ansi-term shell](https://stackoverflow.com/questions/2886184/copy-paste-in-emacs-ansi-term-shell)

## emacs ediff

``` shell
git clone https://github.com/paulotome/emacstool /tmp/emacstool
cd /tmp/emacstool
sudo mv ediff.sh /usr/local/bin/ediff.sh

## change the ediff.sh
## _EMACSCLIENTOPTS="-nw -t"

sudo chmod +x /usr/local/bin/ediff.sh
```

## font

>>>
Global Fonts, .Xresources, and Emacs Daemon
I found that the only way to set fonts so that they remain consistent across emacs -nw, emacs23(-gtk), emacsclient -t, emacsclient -c, was to declare them in .Xresources and .emacs. But the declarations can’t conflict with each other. Otherwise, emacs --daemon complains. It doesn’t like (set-face-attribute ‘default nil :font FONT) and the like. For instance, to use Terminus, 9 pixel size across the board, I needed to insert

Emacs.font: Terminus-9

(set-default-font “Terminus-9”)

in my ~/.Xresources and ~/.emacs, respectively.


copy from [Set Fonts](https://www.emacswiki.org/emacs/SetFonts)

## emacs and rust
see [How to show suggestions for YASnippets when using eglot](https://stackoverflow.com/questions/72601990/how-to-show-suggestions-for-yasnippets-when-using-eglot)
also see [emacs.d](https://github.com/granddaifuku/.emacs.d)

## emacs-plus

``` shell
brew tap d12frosted/emacs-plus
brew install emacs-plus
ln -s /usr/local/opt/emacs-plus/Emacs.app /Applications
```
copy from [Vanilla Emacs with Purcell](https://www.sheerwill.live/posts/main/20220723211325-vanilla_emacs_with_purcell/)
also see [Emacs Plus](https://github.com/d12frosted/homebrew-emacs-plus)
