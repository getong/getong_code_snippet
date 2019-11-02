alias mv='mv -i'
alias rm='trash-put'
alias cp='cp -i'
export LS_OPTIONS='--color=auto'
alias ls='ls $LS_OPTIONS'
export PATH=/usr/local/tmux-2.4/bin:$PATH

alias fbterm="zsh ~/.background_fbterm /usr/share/images/desktop-base/spacefun-grub-widescreen2.png"
tmux_init()
{
    tmux new-session -s "getong" -d #-n "local"     开启一个会话
    tmux new-window #-n "other"           开启一个窗口
    tmux -2 attach-session -d           # tmux -2强制启用256color，连接已开启的tmux
}

# 判断是否已有开启的tmux会话，没有则开启
if [[ $(tty) == \/dev\/tty[1-6]* ]]; then
    # eval `dbus-launch --auto-syntax`
    fcitx > /dev/null 2>&1
    if [ -z `ps aux | grep -i tmux | grep -v grep` ]; then
        fbterm -i fcitx-fbterm -- tmux new-session -s "getong"
    else
        fbterm -i fcitx-fbterm -- tmux attach
    fi
elif which tmux 2>&1 >/dev/null; then
    test -z "$TMUX" && (tmux attach || tmux_init)
fi

alias emacs='emacs -nw'

alias tmux_lock='tmux lock-session'

export PS1=\$


# change the man page output
man() {
    env \
        LESS_TERMCAP_mb=$(printf "\e[1;31m") \
        LESS_TERMCAP_md=$(printf "\e[1;31m") \
        LESS_TERMCAP_me=$(printf "\e[0m") \
        LESS_TERMCAP_se=$(printf "\e[0m") \
        LESS_TERMCAP_so=$(printf "\e[1;44;33m") \
        LESS_TERMCAP_ue=$(printf "\e[0m") \
        LESS_TERMCAP_us=$(printf "\e[1;32m") \
            man "$@"
}

# 7z compress
function 7z_cp() {
	7z a -p -mhe -t7z -m0=lzma -mx=0 -mfb=64 -md=32m -ms=on $1.7z $1 &> /dev/null
}

# 7z decompress
function 7z_dc() {
	7z x $1 &> /dev/null
}


# append absolute path to the filenames
function get_dir_file() {
   find $1 -type f
}

## iperf3
```
# iperf3 -s -p 5202
# iperf3 -c 192.168.89.3 -p 5202
```
see [Docker network performance](https://jtway.co/docker-network-performance-b95bce32b4b9)

## Makefile, continue after a command fails
see [Make: how to continue after a command fails?](https://stackoverflow.com/questions/2670130/make-how-to-continue-after-a-command-fails)

>> To ignore errors in a command line, write a - at the beginning of the line's text (after the initial tab). The - is discarded before the command is passed to the shell for execution.

>> For example,
``` shell
clean:
  -rm -f *.o
```

## ssh-agent and tmux

``` shell
# ~/.bashrc

if [ -z "$TMUX" ]; then
    # we're not in a tmux session

    if [ ! -z "$SSH_TTY" ]; then
        # We logged in via SSH

        # if ssh auth variable is missing
        if [ -z "$SSH_AUTH_SOCK" ]; then
            export SSH_AUTH_SOCK="$HOME/.ssh/.auth_socket"
        fi

        # if socket is available create the new auth session
        if [ ! -S "$SSH_AUTH_SOCK" ]; then
            `ssh-agent -a $SSH_AUTH_SOCK` > /dev/null 2>&1
            echo $SSH_AGENT_PID > $HOME/.ssh/.auth_pid
        fi

        # if agent isn't defined, recreate it from pid file
        if [ -z $SSH_AGENT_PID ]; then
            export SSH_AGENT_PID=`cat $HOME/.ssh/.auth_pid`
        fi

        # Add all default keys to ssh auth
        ssh-add 2>/dev/null

        # start tmux
        tmux attach
    fi
fi
```
and append this into ~/.tmux.conf

``` shell
# ~/.tmux.conf
set -g update-environment -r
```
copy from [TMUX and SSH auto-login with ssh-agent (finally!)](https://development.robinwinslow.uk/2012/07/20/tmux-and-ssh-auto-login-with-ssh-agent-finally/)
