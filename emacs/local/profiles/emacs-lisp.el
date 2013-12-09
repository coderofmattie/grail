;;----------------------------------------------------------------------
;; Emacs Lisp
;;----------------------------------------------------------------------

(defun emacs-lisp-create-repl ( repl-name )
  (with-current-buffer (get-buffer-create repl-name)
    (lisp-interaction-mode)))

(repl-setup-for-langauge "elisp-repl" "elisp-mode" 'emacs-lisp-create-repl)

(repl-setup-for-command  "elisp" "elisp-repl" "elisp-mode" 'emacs-lisp-create-repl)

(setq
  lisp-indent-offset 2)

(defun elisp-list-fn-signatures ()
  (interactive)
  (occur "(defun"))

(add-hook 'emacs-lisp-mode-hook
  (lambda ()
    (configure-for-programming 'elisp-list-fn-signatures "elisp-mode")

    ;; this binding is very important. normal evaluation of defuns such as defvar
    ;; and defcustom do not change the default value because the form only sets
    ;; the value if nil.

    ;; eval-defun will "reset" these forms as well as not echoing into the buffer.
    ;; this function/keybinding should be used exclusively to avoid frustrating
    ;; errors.

    (lisp-smart-parens-editing)

    (configure-for-evaluation 'eval-defun 'eval-last-sexp 'eval-region 'eval-buffer)
    (configure-for-debugging 'edebug-defun)

;;    (dwim-tab-localize-context 'completion-at-point)
    (turn-on-dwim-tab 'lisp-indent-line))
  t)
