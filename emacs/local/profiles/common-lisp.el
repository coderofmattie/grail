;;----------------------------------------------------------------------
;; common lisp
;;----------------------------------------------------------------------
(defvar cl-function-decl ".*(defun.*")

(defun cl-list-functions ()
  (interactive)
  (occur cl-function-decl))

(require 'lisp-mode)

(setq lisp-mode-hook nil)

(add-hook 'lisp-mode-hook
  (lambda ()
    (configure-for-programming 'cl-list-functions "lisp-mode")

    (lisp-smart-parens-editing)

    (turn-on-dwim-tab 'common-lisp-indent-function))
  t)

