;;----------------------------------------------------------------------
;; emacs-lisp.el - emacs lisp mode configuration
;;
;; description:
;;
;; configure emacs lisp with standardized programming features
;;----------------------------------------------------------------------
(require 'lex-cache)

(defconst emacs-lisp-refresh-completion-interval 1)

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

(defun emacs-lisp-function-symbols ()
  (let
    (( name-list ))

    (mapatoms
      (lambda ( sym )
        (when (functionp sym)
          (setq name-list (cons (symbol-name sym) name-list ))) )
      obarray)

    name-list))

(lex-cache dwim-complete/elisp-fn-candidates emacs-lisp-refresh-completion-interval
  (lambda ()
    (emacs-lisp-function-symbols) ))

(defun emacs-lisp-variable-symbols ()
  (let
    (( name-list ))

    (mapatoms
      (lambda ( sym )
        (unless (functionp sym)
          (setq name-list (cons (symbol-name sym) name-list ))) )
      obarray)

    name-list))

(lex-cache dwim-complete/elisp-var-candidates emacs-lisp-refresh-completion-interval
  (lambda ()
    (emacs-lisp-variable-symbols) ))

(defun dwim-complete/emacs-lisp-fn-source ()
  (dwim-complete/make-source "functions"
    (dwim-complete/elisp-fn-candidates)
    (dwim-complete/make-action 'dwim-complete-replace-stem) ))

(defun dwim-complete/emacs-lisp-var-source ()
  (dwim-complete/make-source "variables"
    (dwim-complete/elisp-var-candidates)
    (dwim-complete/make-action 'dwim-complete-replace-stem) ))

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

    (turn-on-dwim-tab 'lisp-indent-line)

    (unless (dwim-complete-mode-check-type major-mode "mode")
      (dwim-complete-mode-add-source major-mode (dwim-complete/emacs-lisp-fn-source))
      (dwim-complete-mode-add-source major-mode (dwim-complete/emacs-lisp-var-source))

      (dwim-complete-mode-add-type major-mode "mode"))

    (dwim-complete/for-buffer) )
  t)
