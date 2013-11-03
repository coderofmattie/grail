;;----------------------------------------------------------------------
;; Emacs Lisp
;;----------------------------------------------------------------------

(setq
  lisp-indent-offset 2)

(defun elisp-list-fn-signatures ()
  (interactive)
  (occur "(defun"))

(add-hook 'emacs-lisp-mode-hook
  (lambda ()
    (swap-paren-keys)

    (configure-for-programming 'elisp-list-fn-signatures "elisp-mode")

    ;; this binding is very important. normal evaluation of defuns such as defvar
    ;; and defcustom do not change the default value because the form only sets
    ;; the value if nil.

    ;; eval-defun will "reset" these forms as well as not echoing into the buffer.
    ;; this function/keybinding should be used exclusively to avoid frustrating
    ;; errors.

    (configure-for-navigation 'forward-sexp 'backward-sexp)

    (configure-for-evaluation 'eval-defun 'eval-last-sexp 'eval-region 'eval-buffer)
    (configure-for-debugging 'edebug-defun)

    (dwim-tab-localize-context 'lisp-complete-symbol)
    (turn-on-dwim-tab 'lisp-indent-line))
  t)
