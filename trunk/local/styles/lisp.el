;;----------------------------------------------------------------------
;; lisp.el
;; Primary Author: Mike Mattie
;; Copyright:
;;----------------------------------------------------------------------

;; fancy paren/delimited highlighting.

(unless
  (robust-load-elisp
    (require 'mic-paren)

    (setq
      paren-showing t
      show-paren-style 'parenthesis
      show-paren-delay 1
      paren-sexp-mode 'match )

    (paren-activate))
  ;; do the repair thingy
  (grail-dup-error-to-scratch "the lisp style is hobbled by the unloadable mic-paren dependency"))

;; basic settings
(setq
  lisp-indent-offset 2)

(defun swap-paren-keys ()
  "bind the parentheses to the brace keys, while the shifted
   paren keys become the braces."
  (interactive)

  ;; make the parentheses a bit easier to type, less shifting.
  (local-set-key (kbd "[") (lambda () (interactive) (insert-char ?\( 1 nil)))
  (local-set-key (kbd "]") (lambda () (interactive) (insert-char ?\) 1 nil)))

  (local-set-key (kbd "(") (lambda () (interactive) (insert-char ?\[ 1 nil)))
  (local-set-key (kbd ")") (lambda () (interactive) (insert-char ?\] 1 nil))) )

(add-hook 'emacs-lisp-mode-hook
  (lambda ()
    (swap-paren-keys)

    ;; this binding is very important. normal evaluation of defuns such as defvar
    ;; and defcustom do not change the default value because the form only sets
    ;; the value if nil.

    ;; eval-defun will "reset" these forms as well as not echoing into the buffer.
    ;; this function/keybinding should be used exclusively to avoid frustrating
    ;; errors.
    (local-set-key (kbd "C-x e") 'eval-defun)

    ;; replace dired at point, far less useful to me than instrumenting a function.
    (local-set-key (kbd "C-x d") 'edebug-defun)

    ;; elisp doesn't need tags, find-function works just fine.
    (local-set-key (kbd "M-.")   'find-function) ))

;;----------------------------------------------------------------------
;;
;;----------------------------------------------------------------------

(eval-after-load "erc"
  (add-hook 'erc-mode-hook 'swap-paren-keys))

