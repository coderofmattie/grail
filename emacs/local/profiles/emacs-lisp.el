;;----------------------------------------------------------------------
;; emacs-lisp.el - emacs lisp mode configuration
;;
;; description:
;;
;; configure emacs lisp with standardized programming features
;;----------------------------------------------------------------------
(require 'lex-cache)
(require 'custom-key)
(require 'borg-repl)

(defconst profile/elisp-name "elisp")
(defconst profile/elisp-repl-name (borg-repl/repl-name profile/elisp-name))

(setq
  lisp-indent-offset 2)

;;
;; key-binding search functions
;;

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


;;
;; dwim tab completion backend
;;

(defconst emacs-lisp-refresh-completion-interval 1)


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
    'dwim-complete/elisp-fn-candidates
    'dwim-complete-replace-stem))

(defun dwim-complete/emacs-lisp-var-source ()
  (dwim-complete/make-source "variables"
    'dwim-complete/elisp-var-candidates
    'dwim-complete-replace-stem))

(defun profile/elisp-repl-new ()
  (interactive)
  (let
    (( new-elisp-repl (get-buffer-create (concat "*" (generate-new-buffer-name "elisp/eval") "*")) ))

    (pop-to-buffer
      (if new-elisp-repl
        (with-current-buffer new-elisp-repl
          (emacs-lisp-mode)
          (current-buffer) )
        (progn
          (message "profile/elisp: cannot create new scratch buffer")
          nil) )) ))

(defun profile/elisp-repl-statement ()
  (interactive)
  (let
    (( current-prefix-arg ))

    (call-interactively 'eval-last-sexp nil)) )

(defun emacs-lisp/profile ()
  (configure-for-programming 'elisp-list-fn-signatures profile/elisp-name)

  (lisp-smart-parens-editing)

  (borg-repl/bind-repl profile/elisp-name
    'profile/elisp-repl-new
    'profile/elisp-repl-statement
    'eval-region
    'eval-buffer
    'eval-defun )

  (custom-key-group "elisp-debug" "d" nil
     ("d" . eval-defun))

  (unless (dwim-complete-mode-check-type profile/elisp-name "mode")
    (dwim-complete-mode-add-source profile/elisp-name (dwim-complete/emacs-lisp-fn-source))
    (dwim-complete-mode-add-source profile/elisp-name (dwim-complete/emacs-lisp-var-source))

    (dwim-complete-mode-add-type profile/elisp-name "mode"))

  (turn-on-dwim-tab 'lisp-indent-line)

  (dwim-complete/for-buffer) )

(add-hook 'emacs-lisp-mode-hook 'emacs-lisp/profile t)

(provide 'profile/emacs-lisp)
