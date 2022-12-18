

;; activate all the packages (in particular autoloads)
(package-initialize)

(setq inhibit-startup-screen t)
(tool-bar-mode -1)
(menu-bar-mode -1)

;; list the repositories containing them
(setq package-archives '(("gnu" . "http://mirrors.ustc.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.ustc.edu.cn/elpa/melpa/")
                         ("nongnu" . "http://mirrors.ustc.edu.cn/elpa/nongnu/")))

;;; copy from [How to automatically install Emacs packages by specifying a list of package names?](https://stackoverflow.com/questions/10092322/how-to-automatically-install-emacs-packages-by-specifying-a-list-of-package-name)
;;; list the packages you want
(setq package-list '(edts company indent-guide pangu-spacing spinner undo-tree highlight-thing markdown-mode switch-window
                          protobuf-mode tide dart-mode dart-server mix csharp-mode omnisharp lua-mode flycheck-rust rust-mode
                          swift-mode lsp-mode which-key use-package rustic magit openwith corfu
                          color-theme-sanityinc-tomorrow olivetti aggressive-indent dap-mode vterm multiple-cursors))

;; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;;(add-to-list 'default-frame-alist '(foreground-color . "white"))
;;(add-to-list 'default-frame-alist '(background-color . "black"))
(require 'color-theme-sanityinc-tomorrow)
(load-theme 'sanityinc-tomorrow-blue t)
;;(color-theme-sanityinc-tomorrow--define-theme blue)
;;(add-to-list 'default-frame-alist '(cursor-color . "black"))
;; (add-to-list 'default-frame-alist '(cursor-type . bar))
;;(blink-cursor-mode -1)
;;(setq blink-cursor-blinks -1)
;; https://protesilaos.com/codelog/2020-07-16-emacs-focused-editing/
(use-package olivetti
  :ensure
  :defer
  :diminish
  :config
  (setq olivetti-body-width 0.65)
  (setq olivetti-minimum-body-width 72)
  (setq olivetti-recall-visual-line-mode-entry-state t)
  (define-minor-mode prot/olivetti-mode
    "Toggle buffer-local `olivetti-mode' with additional parameters.
Fringes are disabled.  The modeline is hidden, except for
`prog-mode' buffers (see `prot/hidden-mode-line-mode').  The
default typeface is set to a proportionately-spaced family,
except for programming modes (see `prot/variable-pitch-mode').
The cursor becomes a blinking bar, per `prot/cursor-type-mode'."
    :init-value nil
    :global nil
    (if prot/olivetti-mode
        (progn
          (olivetti-mode 1)
          (set-window-fringes (selected-window) 0 0)
          (prot/variable-pitch-mode 1)
          (prot/cursor-type-mode 1)
          (unless (derived-mode-p 'prog-mode)
            (prot/hidden-mode-line-mode 1)))
      (olivetti-mode -1)
      (set-window-fringes (selected-window) nil) ; Use default width
      (prot/variable-pitch-mode -1)
      (prot/cursor-type-mode -1)
      (unless (derived-mode-p 'prog-mode)
        (prot/hidden-mode-line-mode -1))))
  :bind ("C-c o" . prot/olivetti-mode))

(use-package emacs
  :commands prot/hidden-mode-line-mode
  :config
  (setq mode-line-percent-position '(-3 "%p"))
  (setq mode-line-defining-kbd-macro
        (propertize " Macro" 'face 'mode-line-emphasis))
  (setq-default mode-line-format
                '("%e"
                  mode-line-front-space
                  mode-line-mule-info
                  mode-line-client
                  mode-line-modified
                  mode-line-remote
                  mode-line-frame-identification
                  mode-line-buffer-identification
                  "  "
                  mode-line-position
                  (vc-mode vc-mode)
                  " "
                  mode-line-modes
                  " "
                  mode-line-misc-info
                  mode-line-end-spaces))
  (define-minor-mode prot/hidden-mode-line-mode
    "Toggle modeline visibility in the current buffer."
    :init-value nil
    :global nil
    (if prot/hidden-mode-line-mode
        (setq-local mode-line-format nil)
      (kill-local-variable 'mode-line-format)
      (force-mode-line-update))))

(use-package face-remap
  :diminish buffer-face-mode            ; the actual mode
  :commands prot/variable-pitch-mode
  :config
  (define-minor-mode prot/variable-pitch-mode
    "Toggle `variable-pitch-mode', except for `prog-mode'."
    :init-value nil
    :global nil
    (if prot/variable-pitch-mode
        (unless (derived-mode-p 'prog-mode)
          (variable-pitch-mode 1))
      (variable-pitch-mode -1))))

(use-package emacs
  :config
  (setq-default scroll-preserve-screen-position t)
  (setq-default scroll-conservatively 1) ; affects `scroll-step'
  (setq-default scroll-margin 0)

  (define-minor-mode prot/scroll-centre-cursor-mode
    "Toggle centred cursor scrolling behaviour."
    :init-value nil
    :lighter " S="
    :global nil
    (if prot/scroll-centre-cursor-mode
        (setq-local scroll-margin (* (frame-height) 2)
                    scroll-conservatively 0
                    maximum-scroll-margin 0.5)
      (dolist (local '(scroll-preserve-screen-position
                       scroll-conservatively
                       maximum-scroll-margin
                       scroll-margin))
        (kill-local-variable `,local))))

  ;; C-c l is used for `org-store-link'.  The mnemonic for this is to
  ;; focus the Line and also works as a variant of C-l.
  :bind ("C-c L" . prot/scroll-centre-cursor-mode))

(use-package display-line-numbers
  :defer
  :config
  ;; Set absolute line numbers.  A value of "relative" is also useful.
  (setq display-line-numbers-type t)
  (define-minor-mode prot/display-line-numbers-mode
    "Toggle `display-line-numbers-mode' and `hl-line-mode'."
    :init-value nil
    :global nil
    (if prot/display-line-numbers-mode
        (progn
          (display-line-numbers-mode 1)
          (hl-line-mode 1))
      (display-line-numbers-mode -1)
      (hl-line-mode -1)))
  :bind ("<f7>" . prot/display-line-numbers-mode))
(use-package frame
  :commands prot/cursor-type-mode
  :config
  (setq-default cursor-type 'bar)
  (setq-default cursor-in-non-selected-windows '(bar . 2))
  (setq-default blink-cursor-blinks 50)
  (setq-default blink-cursor-interval nil) ; 0.75 would be my choice
  (setq-default blink-cursor-delay 0.2)
  (blink-cursor-mode -1)
  (define-minor-mode prot/cursor-type-mode
    "Toggle between static block and pulsing bar cursor."
    :init-value nil
    :global t
    (if prot/cursor-type-mode
        (progn
          (setq-local blink-cursor-interval 0.75
                      cursor-type '(bar . 2)
                      cursor-in-non-selected-windows 'hollow)
          (blink-cursor-mode 1))
      (dolist (local '(blink-cursor-interval
                       cursor-type
                       cursor-in-non-selected-windows))
        (kill-local-variable `,local))
      (blink-cursor-mode -1)))
  ;; copy from https://emacsredux.com/blog/2020/12/04/maximize-the-emacs-frame-on-startup/
  ;; start the initial frame maximized
  (add-to-list 'initial-frame-alist '(fullscreen . maximized))
  ;; start every frame maximized
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  )

;; ispell 中文问题
;; use apsell as ispell backend
(setq-default ispell-program-name "aspell")
;; use American English as ispell default dictionary
(ispell-change-dictionary "american" t)

(setq-default indent-tabs-mode nil)
(setq default-tab-width 4)

(setq dart-server-sdk-path "~/flutter/bin/cache/dart-sdk/")
(setq dart-server-enable-analysis-server t)
(add-hook 'dart-server-hook 'flycheck-mode)

;;参考 http://www.aiuxian.com/article/p-823990.html
;; Linux下emacs如何和X-Window系统共享剪贴板
;;(setq x-select-enable-clipboard t)
;;
;;;; use xsel to copy/paste in emacs-nox
;;(unless window-system
;;  (when (getenv "DISPLAY")
;;	(defun xsel-cut-function (text &optional push)
;;    (with-temp-buffer
;;		(insert text)
;;		(call-process-region (point-min) (point-max) "xsel" nil 0 nil "--clipboard" "--input")))
;;	(defun xsel-paste-function()
;;    (let ((xsel-output (shell-command-to-string "xsel --clipboard --output")))
;;		(unless (string= (car kill-ring) xsel-output)
;;        xsel-output )))
;;	(setq interprogram-cut-function 'xsel-cut-function)
;;	(setq interprogram-paste-function 'xsel-paste-function)
;;	))

(global-superword-mode t)
(global-hl-line-mode t)

;; copy from [hl-line-mode hide background, how to avoid this?](https://emacs.stackexchange.com/questions/10445/hl-line-mode-hide-background-how-to-avoid-this)
(defun my-hl-line-range-function ()
  (cons (line-end-position) (line-beginning-position 2)))
(setq hl-line-range-function #'my-hl-line-range-function)

(when window-system
  (require 'hl-line)
  (set-face-attribute 'hl-line nil :inherit nil :background "light yellow")
  (setq global-hl-line-sticky-flag t)
  (global-hl-line-mode 1))

(global-font-lock-mode t)

(global-linum-mode 1)
(setq linum-format "%3d ")
(add-hook 'prog-mode-hook 'linum-mode)

;;(setq-default indent-tabs-mode nil)
(ido-mode 1)
(setq column-number-mode t)

(setq inhibit-startup-message t) ;; 关闭起动时LOGO
(setq visible-bell t);;关闭出错时的提示声
(global-font-lock-mode t);语法高亮
(show-paren-mode t) ;; 显示括号匹配
(setq show-paren-style 'parenthesis)
(setq mouse-yank-at-point t);;支持中键粘贴
(mouse-avoidance-mode 'animate) ;;光标靠近鼠标指针时，让鼠标指针自动让开，别挡住视线。
(setq appt-issue-message t)

(add-to-list 'auto-mode-alist '("\\.dart\\'" . dart-mode))
;; Load rust-mode when you open `.rs` files
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

(eval-after-load "rust-mode"
  '(setq-default rust-format-on-save t))
(add-hook 'rust-mode-hook (lambda ()
                            (flycheck-rust-setup)
                            (lsp)
                            (flycheck-mode)
			                (yas-minor-mode)
                            ))

(require 'rust-mode)
;;(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
(setq company-tooltip-align-annotations t)


(setq rust-format-on-save t)

(setq backup-directory-alist (quote (("." . "~/.backups"))))
(setq version-control t)
(setq kept-old-versions 2)
(setq kept-new-versions 5)
(setq delete-old-versions t)
(setq backup-directory-alist '(("." . "~/.backups")))
(setq backup-by-copying t)

(global-auto-revert-mode 1)

;; Behave like vi's o command
(defun open-next-line (arg)
  "Move to the next line and then opens a line.
  See also `newline-and-indent'."
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (next-line 1)
  (when newline-and-indent
    (indent-according-to-mode)))

(global-set-key (kbd "C-o") 'open-next-line)

;; Behave like vi's O command
(defun open-previous-line (arg)
  "Open a new line before the current one.
  See also `newline-and-indent'."
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (when newline-and-indent
    (indent-according-to-mode)))

(global-set-key (kbd "M-o") 'open-previous-line)

;; Autoindent open-*-lines
(defvar newline-and-indent t
  "Modify the behavior of the open-*-line functions to cause them to autoindent.")

(setq initial-scratch-message nil)

(global-unset-key (kbd "C-SPC")) ;; 输入法快捷键冲突
(global-set-key (kbd "M-SPC") 'set-mark-command)

(icomplete-mode 99)

(defun split-window-4()
  "Splite window into 4 sub-window"
  (interactive)
  (if (= 1 (length (window-list)))
      (progn (split-window-vertically)
             (split-window-horizontally)
             (other-window 2)
             (split-window-horizontally)
             )
    )
  )

(defun change-split-type (split-fn &optional arg)
  "Change 3 window style from horizontal to vertical and vice-versa"
  (let ((bufList (mapcar 'window-buffer (window-list))))
    (select-window (get-largest-window))
    (funcall split-fn arg)
    (mapcar* 'set-window-buffer (window-list) bufList)))


(defun change-split-type-2 (&optional arg)
  "Changes splitting from vertical to horizontal and vice-versa"
  (interactive "P")
  (let ((split-type (lambda (&optional arg)
                      (delete-other-windows-internal)
                      (if arg (split-window-vertically)
                        (split-window-horizontally)))))
    (change-split-type split-type arg)))


(defun change-split-type-3-v (&optional arg)
  "change 3 window style from horizon to vertical"
  (interactive "P")
  (change-split-type 'split-window-3-horizontally arg))

(defun change-split-type-3-h (&optional arg)
  "change 3 window style from vertical to horizon"
  (interactive "P")
  (change-split-type 'split-window-3-vertically arg))

(defun split-window-3-horizontally (&optional arg)
  "Split window into 3 while largest one is in horizon"
  ;;  (interactive "P")
  (delete-other-windows)
  (split-window-horizontally)
  (if arg (other-window 1))
  (split-window-vertically))

(defun split-window-3-vertically (&optional arg)
  "Split window into 3 while largest one is in vertical"
  ;; (interactive "P")
  (delete-other-windows)
  (split-window-vertically)
  (if arg (other-window 1))
  (split-window-horizontally))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(require 'pangu-spacing)
(global-pangu-spacing-mode 1)

(spinner-start 'vertical-breathing 10)
(spinner-start 'minibox)
(spinner-start 'moon)
(spinner-start 'triangle)

;; 高亮显示选中区域
(transient-mark-mode t)
;; 高亮选中区域颜色
(set-face-attribute 'region nil :background "#666" :foreground "#ffffff")

(indent-guide-global-mode)

(defun switch-to-frame (frame-name)
  (interactive "sFrame name:")
  (let ((frames (frame-list)))
    (catch 'break
      (while frames (let ((frame (car frames)))
                      (if (equal (frame-parameter frame 'name) frame-name)
                          (throw 'break (select-frame-set-input-focus frame))
                        (setq frames (cdr frames))))))))

(global-set-key [f9] 'toggle-menu-bar-mode-from-frame)

;; 关闭所有的buffer
(defun close-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

(setq x-underline-at-descent-line t)

;; 开启全局 Company 补全
(global-company-mode 1)

;;文本解码设置默认为 UTF-8
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)

;; Emacs 自动加载外部修改过的文件
(global-auto-revert-mode 1)

(setq company-idle-delay 0.01)
(setq company-minimum-prefix-length 1)

;; set default tab char's display width to 4 spaces
(setq-default tab-width 4) ; emacs 23.1, 24.2, default to 8

;; 对齐插入空格而不是tab
;; copy from http://stackoverflow.com/questions/22710040/emacs-align-regexp-with-spaces-instead-of-tabs
(defadvice align-regexp (around align-regexp-with-spaces activate)
  (let ((indent-tabs-mode nil))
    ad-do-it))

;;如果有喜欢用鼠标选择emacs文本的同学, 可以试试加上这句配置:
(setq mouse-drag-copy-region t)


(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))

(autoload 'gfm-mode "markdown-mode"
  "Major mode for editing GitHub Flavored Markdown files" t)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

(setq debug-on-error nil)

;; copy from [Undo Tree](https://www.emacswiki.org/emacs/UndoTree)
(global-undo-tree-mode)

(savehist-mode 1)
(setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
(setq savehist-file "~/.emacs.d/savehist")

(require 'highlight-thing)
(global-highlight-thing-mode)

;; copy from https://github.com/dimitri/switch-window
(require 'switch-window)
(global-set-key (kbd "C-x o") 'switch-window)
(global-set-key (kbd "C-x 1") 'switch-window-then-maximize)
(global-set-key (kbd "C-x 2") 'switch-window-then-split-below)
(global-set-key (kbd "C-x 3") 'switch-window-then-split-right)
(global-set-key (kbd "C-x 0") 'switch-window-then-delete)

(global-set-key (kbd "C-x 4 d") 'switch-window-then-dired)
(global-set-key (kbd "C-x 4 f") 'switch-window-then-find-file)
(global-set-key (kbd "C-x 4 m") 'switch-window-then-compose-mail)
(global-set-key (kbd "C-x 4 r") 'switch-window-then-find-file-read-only)

(global-set-key (kbd "C-x 4 C-f") 'switch-window-then-find-file)
(global-set-key (kbd "C-x 4 C-o") 'switch-window-then-display-buffer)

(global-set-key (kbd "C-x 4 0") 'switch-window-then-kill-buffer)

(setq switch-window-shortcut-style 'qwerty)
(setq switch-window-qwerty-shortcuts
      '("a" "s" "d" "f" "j" "k" "l" ";" "w" "e" "i" "o"))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)

(setq tide-format-options '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t :placeOpenBraceOnNewLineForFunctions nil))

;; copy from [Saving persistent undo to a single directory, alist format](https://emacs.stackexchange.com/questions/26993/saving-persistent-undo-to-a-single-directory-alist-format)
(use-package undo-tree
  :defer t
  :diminish undo-tree-mode
  :init (global-undo-tree-mode)
  :custom
  (undo-tree-visualizer-diff t)
  (undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
  (undo-tree-visualizer-timestamps t))

;; copy from [Configuring Emacs for Rust development](https://robert.kra.hn/posts/2021-02-07_rust-with-emacs/)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package rustic
  :ensure
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status)
              ("C-c C-c e" . lsp-rust-analyzer-expand-macro)
              ("C-c C-c d" . dap-hydra)
              ("C-c C-c h" . lsp-ui-doc-glance))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t)))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; for rust-analyzer integration
(use-package lsp-mode
  :ensure
  :commands lsp
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  ;; This controls the overlays that display type and other hints inline. Enable
  ;; / disable as you prefer. Well require a `lsp-workspace-restart' to have an
  ;; effect on open projects.
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints nil)
  (lsp-rust-analyzer-display-reborrow-hints nil)
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))


(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.rust_example\\'")
  ;; or
  (add-to-list 'lsp-file-watch-ignored-files "[/\\\\]\\.rust_example\\'"))
(setq lsp-file-watch-threshold 2000)
(setq lsp-enable-file-watchers nil)

(use-package lsp-ui
  :ensure
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; inline errors

(use-package flycheck :ensure)


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; auto-completion and code snippets

(use-package yasnippet
  :ensure
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))

;;(use-package company
;;  :ensure
;;  :bind
;;  (:map company-active-map
;;              ("C-n". company-select-next)
;;              ("C-p". company-select-previous)
;;              ("M-<". company-select-first)
;;              ("M->". company-select-last))
;;  (:map company-mode-map
;;        ("<tab>". tab-indent-or-complete)
;;        ("TAB". tab-indent-or-complete)))

(defun company-yasnippet-or-completion ()
  (interactive)
  (or (do-yas-expand)
      (company-complete-common)))

(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "::") t nil)))))

(defun do-yas-expand ()
  (let ((yas/fallback-behavior 'return-nil))
    (yas/expand)))

(defun tab-indent-or-complete ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas/minor-mode)
            (null (do-yas-expand)))
        (if (check-expansion)
            (company-complete-common)
          (indent-for-tab-command)))))


(setq tide-format-options '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t :placeOpenBraceOnNewLineForFunctions nil))
;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; Create / cleanup rust scratch projects quickly

(use-package rust-playground :ensure)


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; for Cargo.toml and other config files

(use-package toml-mode :ensure)


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; setting up debugging support with dap-mode

(use-package exec-path-from-shell
  :ensure
  :init (exec-path-from-shell-initialize))

(when (executable-find "lldb-mi")
  (use-package dap-mode
    :ensure
    :config
    (dap-ui-mode)
    (dap-ui-controls-mode 1)

    (require 'dap-lldb)
    (require 'dap-gdb-lldb)
    ;; installs .extension/vscode
    (dap-gdb-lldb-setup)
    (dap-register-debug-template
     "Rust::LLDB Run Configuration"
     (list :type "lldb"
           :request "launch"
           :name "LLDB::Run"
	       :gdbpath "rust-lldb"
           ;; uncomment if lldb-mi is not in PATH
           ;; :lldbmipath "path/to/lldb-mi"
           ))))

;; copy from [Playing nicely with linum](https://www.emacswiki.org/emacs/UndoTree)
(defun undo-tree-visualizer-update-linum (&rest args)
  (linum-update undo-tree-visualizer-parent-buffer))
(advice-add 'undo-tree-visualize-undo :after #'undo-tree-visualizer-update-linum)
(advice-add 'undo-tree-visualize-redo :after #'undo-tree-visualizer-update-linum)
(advice-add 'undo-tree-visualize-undo-to-x :after #'undo-tree-visualizer-update-linum)
(advice-add 'undo-tree-visualize-redo-to-x :after #'undo-tree-visualizer-update-linum)
(advice-add 'undo-tree-visualizer-mouse-set :after #'undo-tree-visualizer-update-linum)
(advice-add 'undo-tree-visualizer-set :after #'undo-tree-visualizer-update-linum)

;; copy from [Aligning columns in Emacs](https://blog.lambda.cx/posts/emacs-align-columns/)
(defun align-non-space (BEG END)
  "Align non-space columns in region BEG END."
  (interactive "r")
  (align-regexp BEG END "\\(\\s-*\\)\\S-+" 1 1 t))

;; copy from [Emacs and symbolic links](https://stackoverflow.com/questions/15390178/emacs-and-symbolic-links)
(setq vc-follow-symlinks t)

;; copy from [How to configure dired to update instantly when files/folders change?](https://www.reddit.com/r/emacs/comments/1acg6q/how_to_configure_dired_to_update_instantly_when/)
(setq global-auto-revert-non-file-buffers t)
(global-auto-revert-mode)

;; copy from https://christiantietze.de/posts/2021/06/emacs-trash-file-macos/
(setq delete-by-moving-to-trash t)
(setq trash-directory "/backup/.Trash-1000/files")  ;; fallback for `move-file-to-trash'
(when (memq window-system '(mac ns))
  (defun system-move-file-to-trash (path)
    "Moves file at PATH to the macOS Trash according to `move-file-to-trash' convention.
Relies on the command-line utility 'trash' to be installed.
Get it from:  <http://hasseg.org/trash/>"
    (shell-command (concat "trash -vF \"" path "\""
                           "| sed -e 's/^/Trashed: /'")
                   nil ;; Name of output buffer
                   "*Trash Error Buffer*")))

;; copy from https://www.danielde.dev/blog/emacs-for-swift-development
(defun print-swift-var-under-point()
  (interactive)
  (if (string-match-p (string (preceding-char)) "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_")
      (backward-sexp)
    nil)
  (kill-sexp)
  (yank)
  (move-end-of-line nil)
  (newline)
  (insert "print(\"")
  (yank)
  (insert ": \\(")
  (yank)
  (insert ")\")")
  (indent-for-tab-command))
(use-package swift-mode
  :bind (("C-c l" . print-swift-var-under-point)))

(defun xcode-build()
  (interactive)
  (shell-command-to-string
   "osascript -e 'tell application \"Xcode\"' -e 'set targetProject to active workspace document' -e 'build targetProject' -e 'end tell'"))
(defun xcode-run()
  (interactive)
  (shell-command-to-string
   "osascript -e 'tell application \"Xcode\"' -e 'set targetProject to active workspace document' -e 'stop targetProject' -e 'run targetProject' -e 'end tell'"))
(defun xcode-test()
  (interactive)
  (shell-command-to-string
   "osascript -e 'tell application \"Xcode\"' -e 'set targetProject to active workspace document' -e 'stop targetProject' -e 'test targetProject' -e 'end tell'"))
(global-set-key (kbd "C-c p b") 'xcode-build)
(global-set-key (kbd "C-c p r") 'xcode-run)
(global-set-key (kbd "C-c p t") 'xcode-test)

(defun xcode-open-current-file()
  (interactive)
  (shell-command-to-string
   (concat "open -a \"/Applications/Xcode.app\" " (buffer-file-name)))
  (kill-new (car (cdr (split-string (what-line)))))
  (shell-command-to-string
   "open keysmith://run-shortcut/796BB627-5433-48E4-BB54-1AA6C54A14E8"))
(global-set-key (kbd "C-c p o") 'xcode-open-current-file)

(defun insert-todo-comment ()
  (interactive)
  (indent-for-tab-command)
  (insert "TODO: ")
  (back-to-indentation)
  (set-mark-command nil)
  (move-end-of-line nil)
  (comment-dwim nil))
(defun todo-comment-on-next-line ()
  "Insert a TODO comment on the next line at the proper indentation"
  (interactive)
  (move-end-of-line nil)
  (newline)
  (insert-todo-comment))
(add-hook 'prog-mode-hook (lambda () (local-set-key (kbd "") #'todo-comment-on-next-line)))

;; copy from https://xenodium.com/emacs-create-a-swift-packageproject/
(defun ar/swift-package-init ()
  "Execute `swift package init', with optional name and completing type."
  (interactive)
  (let* ((name (read-string "name (default): "))
         (type (completing-read
                "project type: "
                ;; Splits "--type empty|library|executable|system-module|manifest"
                (split-string
                 (nth 1 (split-string
                         (string-trim
                          (seq-find
                           (lambda (line)
                             (string-match "--type" line))
                           (process-lines "swift" "package" "init" "--help")))
                         "   "))
                 "|")))
         (command (format "swift package init --type %s" type)))
    (unless (string-empty-p name)
      (append command "--name " name))
    (shell-command command))
  (dired default-directory)
  (revert-buffer))
;; copy from [How to configure dired to update instantly when files/folders change?](https://www.reddit.com/r/emacs/comments/1acg6q/how_to_configure_dired_to_update_instantly_when/)
(setq global-auto-revert-non-file-buffers t)
(global-auto-revert-mode)


;; copy from https://emacs-lsp.github.io/lsp-mode/page/installation/
(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (XXX-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

;; optionally
(use-package lsp-ui :commands lsp-ui-mode)
;; if you are helm user
(use-package helm-lsp :commands helm-lsp-workspace-symbol)
;; if you are ivy user
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; optionally if you want to use debugger
(use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; optional if you want which-key integration
(use-package which-key
  :config
  (which-key-mode))

;; copy from https://zenn.dev/yukit/articles/25a88b33a35633
(add-to-list 'exec-path (expand-file-name "/backup/backup/rust_installation/cargo/bin"))
(add-to-list 'exec-path (expand-file-name "/backup/backup/rust_installation/rustup/toolchains/nightly-x86_64-apple-darwin/bin"))
(eval-after-load "rust-mode"
  '(setq-default rust-format-on-save t))
(add-hook 'rust-mode-hook (lambda ()
                            (flycheck-rust-setup)
                            (lsp)
                            (flycheck-mode)
			                (yas-minor-mode)
                            ))

;; copy from [Rust development environment for Emacs](https://rustrepo.com/repo/brotzeit-rustic-rust-ides)
(defun rustic-mode-auto-save-hook ()
  "Enable auto-saving in rustic-mode buffers."
  (when buffer-file-name
    (setq-local compilation-ask-about-save nil)))
(add-hook 'rustic-mode-hook 'rustic-mode-auto-save-hook)
;; (setq rustic-lsp-server 'rls)
(setq rustic-lsp-server 'rust-analyzer)
(setq rustic-analyzer-command '("/backup/backup/rust_installation/rustup/toolchains/nightly-x86_64-unknown-linux-gnu/bin/rust-analyzer"))
(setq lsp-rust-analyzer-server-command '("/backup/backup/rust_installation/rustup/toolchains/nightly-x86_64-unknown-linux-gnu/bin/rust-analyzer"))
(setq rustic-lsp-client 'lsp-mode)
(with-eval-after-load "lsp-rust"
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection
                     (lambda ()
                       `(,(or (executable-find
                               (cl-first lsp-rust-analyzer-server-command))
                              (lsp-package-path 'rust-analyzer)
                              "rust-analyzer")
                         ,@(cl-rest lsp-rust-analyzer-server-args))))
    :remote? t
    :major-modes '(rust-mode rustic-mode)
    :initialization-options 'lsp-rust-analyzer--make-init-options
    :notification-handlers (ht<-alist lsp-rust-notification-handlers)
    :action-handlers (ht ("rust-analyzer.runSingle" #'lsp-rust--analyzer-run-single))
    :library-folders-fn (lambda (_workspace) lsp-rust-library-directories)
    :after-open-fn (lambda ()
                     (when lsp-rust-analyzer-server-display-inlay-hints
                       (lsp-rust-analyzer-inlay-hints-mode)))
    :ignore-messages nil
    :server-id 'rust-analyzer-remote)))
(defun start-file-process-shell-command@around (start-file-process-shell-command name buffer &rest args)
  "Start a program in a subprocess.  Return the process object for it. Similar to `start-process-shell-command', but calls `start-file-process'."
  ;; On remote hosts, the local `shell-file-name' might be useless.
  (let ((command (mapconcat 'identity args " ")))
    (funcall start-file-process-shell-command name buffer command)))
(advice-add 'start-file-process-shell-command :around #'start-file-process-shell-command@around)
(push 'rustic-clippy flycheck-checkers)

;; copy from https://immerrr.github.io/lua-mode/
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))


(defun pnh-lua-completion-string-for (expr file)
  (mapconcat 'identity
             `("do"
               "local clone = function(t)"
               "  local n = {} for k,v in pairs(t) do n[k] = v end return n"
               "end"
               "local function cpl_for(input_parts, ctx, prefixes)"
               "  if #input_parts == 0 and ctx ~= _G then"
               "    return ctx"
               "  elseif #input_parts == 1 then"
               "    local matches = {}"
               "    for k in pairs(ctx) do"
               "      if k:find('^' .. input_parts[1]) then"
               "        local parts = clone(prefixes)"
               "        table.insert(parts, k)"
               "        table.insert(matches, table.concat(parts, '.'))"
               "      end"
               "    end"
               "    return matches"
               "  else"
               "    local token1 = table.remove(input_parts, 1)"
               "    table.insert(prefixes, first_part)"
               "    return cpl_for(input_parts, ctx[token1], prefixes)"
               "  end"
               "end"
               "local i = {" ,@(mapcar (apply-partially 'format "'%s',")
                                       (split-string expr "\\.")) "}"
               ,(format "local f = io.open('%s', 'w')" file)
               ;; TODO: using _G here is pretty lame! try to get local context
               "for _,l in ipairs(cpl_for(i, _G, {})) do"
               "  f:write(l .. string.char(10))"
               "end"
               "f:close()"
               "end") "\n"))

(defun pnh-lua-complete ()
  (let* ((boe (save-excursion (search-backward-regexp "[^\.a-zA-Z0-9_]")
                              (point)))
         (bot (save-excursion (when (symbol-at-point)
                                (backward-word)) (point)))
         (expr (buffer-substring-no-properties (1+ boe) (point)))
         (file (make-temp-file "lua-completions-")))
    (lua-send-string (pnh-lua-completion-string-for expr file))
    (sit-for 0.1)
    (list bot (point) (when (file-exists-p file)
                        (with-temp-buffer
                          (insert-file-contents file)
                          (delete-file file)
                          (butlast (split-string (buffer-string) "\n")))))))

(add-hook 'lua-mode-hook
          (defun pnh-lua-mode-hook ()
            (make-variable-buffer-local 'completion-at-point-functions)
            (add-to-list 'completion-at-point-functions 'pnh-lua-complete)))
;;Generated by Phil Hagelberg using scpaste at Mon Jun 6 10:41:14 2016. ICT. (original)


;; cpp mode https://linuxhint.com/c_emacs_configuration/
(use-package auto-complete
  :ensure t
  :init
  (progn
    (ac-config-default)
    (global-auto-complete-mode t)
    ))


(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode t))
(use-package modern-cpp-font-lock :ensure t)
(use-package magit
  :ensure t
  :init
  (progn
    (bind-key "C-x g" 'magit-status)))
(defun code-compile ()
  (interactive)
  (unless (file-exists-p "Makefile")
    (set (make-local-variable 'compile-command)
         (let ((file (file-name-nondirectory buffer-file-name)))
           (format "%s -o %s %s"
                   (if  (equal (file-name-extension file) "cpp") "g++" "gcc" )
                   (file-name-sans-extension file)
                   file)))
    (compile compile-command)))

(global-set-key [f9] 'code-compile)


;; copy from http://xahlee.info/emacs/emacs/emacs_set_default_font_size.html
(defun xah-set-default-font-size ()
  "Set default font globally.
Note, this command change font size only for current session, not in init file.
This command useful for making font large when you want to do video livestream.
URL `http://xahlee.info/emacs/emacs/emacs_set_default_font_size.html'
Version: 2021-07-26 2021-08-21 2022-08-05"
  (interactive)
  (let (($fSize (read-string "size:" "16" nil "16")))
    (if (> (string-to-number $fSize) 51)
        (user-error "Max font size allowed is 51. You gave %s " $fSize)
      (set-frame-font
       (cond
        ((string-equal system-type "windows-nt")
         (if (member "Consolas" (font-family-list)) (format "Consolas-%s" $fSize) nil))
        ((string-equal system-type "darwin")
         (if (member "LXGW WenKai Mono" (font-family-list)) "LXGW WenKai Mono" nil))
        ((string-equal system-type "gnu/linux")
         (if (member "DejaVu Sans Mono" (font-family-list)) "DejaVu Sans Mono" nil))
        (t nil))
       t t))))


;; copy from https://emacs.stackexchange.com/questions/57752/mac-option-key-works-as-meta-key-in-terminal-emacs-but-does-not-work-in-gui
;;(when (eq system-type 'darwin)
(use-package term/ns-win
  :config
  (setq mac-option-modifier 'meta))

;; copy from https://emacs-china.org/t/vterm-zsh/20497
;;; Terminal
(use-package vterm
  :when (memq window-system '(mac ns x pgtk))
  :bind (:map vterm-mode-map
              ("C-y" . vterm-yank)
              ("M-y" . vterm-yank-pop)
              ("C-k" . vterm-send-C-k-and-kill))
  :init
  (setq vterm-shell "zsh")
  :config
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")
  (setq vterm-max-scrollback 99999)
  (setq vterm-always-compile-module t)
  (defun vterm-send-C-k-and-kill ()
    "Send `C-k' to libvterm, and put content in kill-ring."
    (interactive)
    (kill-ring-save (point) (vterm-end-of-line))
    (vterm-send-key "k" nil nil t)))

(use-package vterm-toggle
  :when (memq window-system '(mac ns x pgtk))
  :bind (([f8] . vterm-toggle)
         ([f9] . vterm-compile)
         :map vterm-mode-map
         ([f8] . vterm-toggle)
         ([(control return)] . vterm-toggle-insert-cd))
  :config
  (setq vterm-toggle-cd-auto-create-buffer nil)
  (defvar vterm-compile-buffer nil)
  (defun vterm-compile ()
    "Compile the program including the current buffer in `vterm'."
    (interactive)
    (setq compile-command (compilation-read-command compile-command))
    (let ((vterm-toggle-use-dedicated-buffer t)
          (vterm-toggle--vterm-dedicated-buffer (if (vterm-toggle--get-window)
                                                    (vterm-toggle-hide)
                                                  vterm-compile-buffer)))
      (with-current-buffer (vterm-toggle-cd)
        (setq vterm-compile-buffer (current-buffer))
        (rename-buffer "*vterm compilation*")
        (compilation-shell-minor-mode 1)
        (vterm-send-M-w)
        (vterm-send-string compile-command t)
        (vterm-send-return)))))

;; copy from https://www.emacswiki.org/emacs/SystemTrash
(setq delete-by-moving-to-trash t)
(defun system-move-file-to-trash (file)
  "Use \"trash\" to move FILE to the system trash.
When using Homebrew, install it using \"brew install trash-cli\"."
  (call-process (executable-find "trash-put")
		        nil 0 nil
		        file))

;; copy from https://lucidmanager.org/productivity/manage-files-with-emacs/
;; Open dired folders in same buffer
(put 'dired-find-alternate-file 'disabled nil)
;; Copy and move files netween dired buffers
(setq dired-dwim-target t)
;; Only y/n answers
(defalias 'yes-or-no-p 'y-or-n-p)
;; below will cause dired mode error in macos,
;; Sort Dired buffers
;;(setq dired-listing-switches "-agho --group-directories-first")

;; copy from https://github.com/d12frosted/homebrew-emacs-plus/issues/383
(when (eq system-type 'darwin)
  (setq insert-directory-program "/usr/local/bin/gls"))
(setq insert-directory-program "gls" dired-use-ls-dired t)
(setq dired-listing-switches "-al --group-directories-first")

;; copy from https://github.com/NapoleonWils0n/ubuntu-dotfiles/blob/f061fa1b05c5ccba8f6b4a2d165660ab8ab3c56b/.config/emacs/init.el#L170
;; openwth
(require 'mm-util)
(add-to-list 'mm-inhibit-file-name-handlers 'openwith-file-handler)

(require 'openwith)
(setq openwith-associations
      (list
       (list (openwith-make-extension-regexp
              '("mpg" "mpeg" "mp3" "mp4" "m4v"
                "avi" "wmv" "wav" "mov" "flv"
                "ogm" "ogg" "mkv" "webm"))
             "VLC"
             '(file))
       (list (openwith-make-extension-regexp
              '("xbm" "pbm" "pgm" "ppm" "pnm"
                "png" "gif" "bmp" "tif" "jpeg" "jpg" "webp"))
             "nsxiv -a"
             '(file))
       (list (openwith-make-extension-regexp
              '("pdf"))
             "zathura"
             '(file))))

(openwith-mode 1)

(use-package corfu
  ;; Optional customizations
  ;; :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect-first nil)    ;; Disable candidate preselection
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-excluded-modes'.
  :init
  (global-corfu-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))

;;(add-to-list 'load-path (expand-file-name "/Users/gerald/personal_infos/vterm-capf"))
;;(setq vterm-capf-frontend 'company)
;;(add-hook 'vterm-mode-hook #'vterm-capf-mode)
;;(global-company-mode)

;; copy from https://emacs-china.org/t/magit-emacs-terminal-proxy/16942/2
(defun proxy-socks-show ()
  "Show SOCKS proxy."
  (interactive)
  (when (fboundp 'cadddr)
    (if (bound-and-true-p socks-noproxy)
        (message "Current SOCKS%d proxy is %s:%d"
                 (cadddr socks-server) (cadr socks-server) (caddr socks-server))
      (message "No SOCKS proxy"))))

(defun proxy-socks-enable ()
  "Enable SOCKS proxy."
  (interactive)
  (require 'socks)
  (setq url-gateway-method 'socks
        socks-noproxy '("localhost")
        socks-server '("Default server" "127.0.0.1" 10808 5))
  (setenv "all_proxy" "socks5://127.0.0.1:10808")
  (proxy-socks-show))

(defun proxy-socks-disable ()
  "Disable SOCKS proxy."
  (interactive)
  (require 'socks)
  (setq url-gateway-method 'native
        socks-noproxy nil)
  (setenv "all_proxy" "")
  (proxy-socks-show))

(defun proxy-socks-toggle ()
  "Toggle SOCKS proxy."
  (interactive)
  (require 'socks)
  (if (bound-and-true-p socks-noproxy)
      (proxy-socks-disable)
    (proxy-socks-enable)))

(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)

;; copy from https://www.reddit.com/r/emacs/comments/iu0euj/getting_modern_multiple_cursors_in_emacs/
(use-package multiple-cursors
  :ensure   t
  :bind (("H-SPC" . set-rectangular-region-anchor)
         ("C-M-SPC" . set-rectangular-region-anchor)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C->" . mc/mark-all-like-this)
         ("C-c C-SPC" . mc/edit-lines)
         ))
