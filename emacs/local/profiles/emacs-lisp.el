;;----------------------------------------------------------------------
;; emacs-lisp.el - emacs lisp mode configuration
;;
;; description:
;;
;; configure emacs lisp with standardized programming features
;;----------------------------------------------------------------------
(require 'lex-cache)
(require 'custom-key)

(defconst emacs-lisp-refresh-completion-interval 1)

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
    'dwim-complete/elisp-fn-candidates
    'dwim-complete-replace-stem))

(defun dwim-complete/emacs-lisp-var-source ()
  (dwim-complete/make-source "variables"
    'dwim-complete/elisp-var-candidates
    'dwim-complete-replace-stem))

(defun emacs-lisp/profile ()
  (configure-for-programming 'elisp-list-fn-signatures "elisp-mode")

  (lisp-smart-parens-editing)

  (custom-key-group "elisp-eval" "e" nil
     ("d" . eval-defun)
     ("e" . eval-last-sexp)
     ("r" . eval-region)
     ("b" . eval-buffer))

  (custom-key-group "elisp-debug" "d" nil
     ("d" . eval-defun))

  (unless (dwim-complete-mode-check-type major-mode "mode")
    (dwim-complete-mode-add-source major-mode (dwim-complete/emacs-lisp-fn-source))
    (dwim-complete-mode-add-source major-mode (dwim-complete/emacs-lisp-var-source))

    (dwim-complete-mode-add-type major-mode "mode"))

  (turn-on-dwim-tab 'lisp-indent-line)

  (dwim-complete/for-buffer) )

(add-hook 'emacs-lisp-mode-hook 'emacs-lisp/profile t)

(provide 'grail/emacs-lisp)
