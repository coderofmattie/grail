;;----------------------------------------------------------------------
;; scheme.el - support for the scheme programming language
;;
;; description:
;;
;; support for scheme programming
;;----------------------------------------------------------------------
(require 'remap-assoc-mode)

(grail-load 'geiser (grail-define-installer "geiser"
                    "git"
                    "git://git.sv.gnu.org/geiser.git"))

(defconst scheme-profile-geiser-scheme-dir (car (grail-find-package-resource "geiser" "scheme")))

(defconst scheme-profile-geiser-racket-dir (concat scheme-profile-geiser-scheme-dir "/racket/"))

(setq-default
  geiser-active-implementations '(racket)
  geiser-default-implementation "racket"
  geiser-racket-collects (list scheme-profile-geiser-racket-dir)
  geiser-racket-init-file (concat grail-interpreters-path "/racket/geiser.rkt")
  geiser-repl-query-on-kill-p nil)

;;----------------------------------------------------------------------
;; auto mode list additions
;;----------------------------------------------------------------------

(setq
  auto-mode-alist (append '(("\\.rkt$" . scheme-mode)) auto-mode-alist ))

;;----------------------------------------------------------------------
;; create standard configure-for-programming functions
;;----------------------------------------------------------------------

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

;;----------------------------------------------------------------------
;; mode hooks
;;----------------------------------------------------------------------

(add-hook 'scheme-mode-hook
  (lambda ()
    (lisp-smart-parens-editing)

    (configure-for-programming 'scheme-list-fn-signatures "scheme-mode")

    ;; this is borked because these functions autoload!

    ;; (configure-for-evaluation
    ;;   'geiser-eval-definition
    ;;   'geiser-eval-last-sexp
    ;;   'geiser-eval-region
    ;;   'geiser-eval-buffer)

    (turn-on-geiser-mode) )
  t)

