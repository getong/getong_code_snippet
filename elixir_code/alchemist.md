#alchemist
Install alchemist via package install, and also install company mode, which enable auto complete.
when edit elixir source file, enable alchemist-mode by hand.
add this two code in your init.el file
```
(add-to-list 'auto-mode-alist '("\\.ex?$" . elixir-mode))
(add-to-list 'auto-mode-alist '("\\.exs?$" . elixir-mode))
(require 'elixir-mode)
(require 'alchemist)
```
