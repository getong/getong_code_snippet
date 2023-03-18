# lisp

## learn lisp in y minutes
see [Learn X in Y minutes](https://learnxinyminutes.com/docs/elisp/)

## type-of function

``` emacs-lisp
(type-of 1)
```
copy from https://stackoverflow.com/questions/10900600/how-to-determine-the-datatype-of-a-variable

## cons cell

``` emacs-lisp
'(1 . 2)                                ; => (1 . 2)
'(?a . 1)                               ; => (97 . 1)
'(1 . "a")                              ; => (1 . "a")
'(1 . nil)                              ; => (1)
'(nil . nil)                            ; => (nil)
```
copy from https://smacs.github.io/elisp/05-cons-cell.html

## Convert Integer to Float

``` emacs-lisp
;; int to float
(float 3) ; 3.0

(truncate 3.3) ; 3
(floor 3.3) ; 3
(ceiling 3.3) ; 4
(round 3.4) ; 3
(string-to-number "3") ; 3
(number-to-string 3) ; "3"
```
copy from http://xahlee.info/emacs/emacs/elisp_convert_int_float_string.html

## Converting Between Symbols and Strings

``` emacs-lisp
(symbol-name 'some-symbol) ; => "some-symbol"
(intern "some-symbol") ; => some-symbol
```
copy from https://emacsredux.com/blog/2014/12/05/converting-between-symbols-and-strings/


## use-package example

``` emacs-lisp
(global-company-mode t)
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(setq company-idle-delay 0.0)
```
convert to be:

``` emacs-lisp
use-package company
  :bind (:map company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous))
  :config
  (setq company-idle-delay 0.3)
  (global-company-mode t))
```
copy from [Configuring Emacs from Scratch — use-package](https://medium.com/helpshift-engineering/configuring-emacs-from-scratch-use-package-c30382297877)