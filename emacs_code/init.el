; copy from [How to automatically install Emacs packages by specifying a list of package names?](https://stackoverflow.com/questions/10092322/how-to-automatically-install-emacs-packages-by-specifying-a-list-of-package-name)
; list the packages you want
(setq package-list '(edts company indent-guide pangu-spacing spinner undo-tree highlight-thing markdown-mode switch-window protobuf-mode elixir-mode alchemist tide dart-mode dart-server mix csharp-mode omnisharp lua-mode racer flycheck-rust rust-mode))

; list the repositories containing them
(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

; activate all the packages (in particular autoloads)
(package-initialize)

; fetch the list of packages available
(unless package-archive-contents
    (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
    (unless (package-installed-p package)
          (package-install package)))

;;设定光标为短线
(setq-default cursor-type 'bar)

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
;;(set-face-background 'hl-line "blue")
;;(set-face-foreground 'highlight nil)
(set-face-background 'hl-line "#BEBEBE")
(set-face-foreground 'hl-line "#0000000")

(blink-cursor-mode 0)
(setq blink-cursor-blinks 0)
(setq-default cursor-type 'bar);光标显示为一竖线
(set-cursor-color "blue")
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

(add-to-list 'auto-mode-alist '("\\.erl?$" . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.hrl?$" . erlang-mode))

(add-to-list 'auto-mode-alist '("\\.ex?$" . elixir-mode))
(add-to-list 'auto-mode-alist '("\\.exs?$" . elixir-mode))
(add-to-list 'auto-mode-alist '("\\.dart\\'" . dart-mode))

(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

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
                                        ;  (interactive "P")
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


(add-hook 'after-init-hook 'my-after-init-hook)
(defun my-after-init-hook ()
 (require 'edts-start))


(setq erlang-root-dir "/usr/local/otp_src_23.0.1/lib/erlang")
(setq erlang-man-root "/usr/local/otp_src_23.0.1/lib/erlang")

;; 关闭文件滑动控件
;;(scroll-bar-mode -1)

;; 开启全局 Company 补全
(global-company-mode 1)

;;文本解码设置默认为 UTF-8
(set-language-environment "UTF-8")

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

(setq load-path (cons "/usr/local/otp_src_23.0.1/lib/erlang/lib/tools-3.4/emacs"
                      load-path))
(setq exec-path (cons "/usr/local/otp_src_23.0.1/bin" exec-path))
(require 'erlang-start)
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

(add-to-list 'auto-mode-alist '("\\.ex?$" . elixir-mode))
(add-to-list 'auto-mode-alist '("\\.exs?$" . elixir-mode))
(require 'elixir-mode)
(require 'alchemist)

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
