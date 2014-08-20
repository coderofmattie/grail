;;----------------------------------------------------------------------
;; scheme.el - support for the scheme programming language
;;
;; description:
;;
;; support for scheme programming
;;----------------------------------------------------------------------
(require 'dwim-tab)
(require 'remap-assoc-mode)
(require 'borg-repl)
(require 'programming-generic)

(defconst profile/scheme-name "scheme")
(defconst profile/scheme-repl-name (borg-repl/repl-name profile/scheme-name))

;;
;; search stuff
;;

;; the atom definition is tweaked for regex purpose. Without including
;; the list symbols the regex would run over lists in it's quest for
;; whitespace.

(defconst scheme-regex-whitespace "[ \n\t]" "whitespace in scheme")
(defconst scheme-regex-atom "[^ \n\t()]+" "whitespace in scheme")

;;
;; file associations
;;

(setq
  auto-mode-alist (append '(("\\.rkt$" . scheme-mode)) auto-mode-alist ))


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

(defun profile/scheme-mode-setup ()
  (programming-mode-generic 'scheme-list-fn-signatures)

  (buffer-ring/add profile/scheme-name)
  (buffer-ring/local-keybindings)

  (grail-require profile/syntax-tools
    "emacs-lisp"
    "syntax"

    (profile/syntax-tools-mode-setup)
    (profile/syntax-tools-lisp) )

  (turn-on-dwim-tab 'lisp-indent-line) )

(add-hook 'scheme-mode-hook 'profile/scheme-mode-setup t)

;;
;; geiser repl
;;

(grail-load-package 'geiser "git" "git://git.sv.gnu.org/geiser.git")

;;
;; need scheme code for repl side inside distribution
;;

(defconst scheme-profile-geiser-scheme-dir (grail-package-resource "geiser" "scheme"))
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

(defun profile/scheme-repl-setup ()
  (interactive)

  (borg-repl/bind-repl cl-repl-name
    'geiser
    'geiser-eval-last-sexp
    'geiser-eval-region
    'geiser-eval-buffer
    'geiser-eval-definition )

  (borg-repl/bind-connect 'geiser-connect)

  (turn-on-geiser-mode) )

(add-hook 'scheme-mode-hook 'profile/scheme-repl-setup t)

(provide 'profile/scheme)


