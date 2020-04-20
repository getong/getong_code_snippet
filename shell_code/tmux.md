# tmux

## prefix key

``` shell
unbind C-b
set -g prefix `
bind-key ` send-prefix
```

or

``` shell
unbind-key C-b
set -g prefix 'C-\'
bind-key 'C-\' send-prefix
```

copy from [What's the least conflicting prefix/escape sequence for screen or tmux?](https://superuser.com/questions/74492/whats-the-least-conflicting-prefix-escape-sequence-for-screen-or-tmux)
