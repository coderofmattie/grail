;;----------------------------------------------------------------------
;; scheme.el - support for the scheme programming language
;;
;; description:
;;
;; support for scheme programming
;;----------------------------------------------------------------------
(require 'remap-assoc-mode)
(require 'borg-repl)

(defconst profile/scheme-name "scheme")
(defconst profile/scheme-repl-name (borg-repl/repl-name profile/scheme-name))

(grail-load-package 'geiser "git" "git://git.sv.gnu.org/geiser.git")

;;
;; file associations
;;

(setq
  auto-mode-alist (append '(("\\.rkt$" . scheme-mode)) auto-mode-alist ))

;;
;; need scheme code for repl side inside distribution
;;
(defconst scheme-profile-geiser-scheme-dir (car (grail-find-package-resource "geiser" "scheme")))
(defconst scheme-profile-geiser-racket-dir (concat scheme-profile-geiser-scheme-dir "/racket/"))

;;
;; defaults
;;

(setq-default
  geiser-active-implementations '(racket)
  geiser-default-implementation "racket"
  geiser-racket-collects (list scheme-profile-geiser-racket-dir)
  geiser-racket-init-file (concat grail-interpreters-path "/racket/geiser.rkt")
  geiser-repl-query-on-kill-p nil)

;;
;; search stuff
;;

;; the atom definition is tweaked for regex purpose. Without including
;; the list symbols the regex would run over lists in it's quest for
;; whitespace.

(defconst scheme-regex-whitespace "[ \n\t]" "whitespace in scheme")
(defconst scheme-regex-atom "[^ \n\t()]+" "whitespace in scheme")

(defconst scheme-function-regex (concat
                                  "("
                                  scheme-regex-whitespace "*"
                                  "define"
                                  scheme-regex-whitespace "+"
                                  "("
                                  scheme-regex-whitespace "*"
                                  scheme-regex-atom
                                  scheme-regex-whitespace "+"))

(defun scheme-list-fn-signatures ()
  (interactive)
  (occur scheme-function-regex))

;;
;; REPL mode hooks
;;

(defadvice geiser (after profile-scheme-advice-geiser
                    (&rest ignore) )
  (buffer-ring/add profile/scheme-repl-name)
  (buffer-ring/local-keybindings) )

(ad-activate 'geiser)

;;
;; language mode hooks.
;;

(defun profile/scheme-run-geiser ()
  (interactive)
  (call-interactively 'geiser))

(defun profile/scheme-mode-setup ()
  (lisp-smart-parens-editing)

  (configure-for-programming 'scheme-list-fn-signatures profile/scheme-name)

  (borg-repl/bind-repl cl-repl-name
    'geiser
    'geiser-eval-last-sexp
    'geiser-eval-region
    'geiser-eval-buffer
    'geiser-eval-definition )

  (turn-on-geiser-mode) )



(add-hook 'scheme-mode-hook 'profile/scheme-mode-setup t)

(provide 'profile/scheme)


