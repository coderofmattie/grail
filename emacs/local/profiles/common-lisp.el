;;----------------------------------------------------------------------
;; common lisp
;;----------------------------------------------------------------------
(defvar cl-function-decl ".*(defun.*")

(defconst cl-lisp-name "cl-lisp")

(defun cl-list-functions ()
  (interactive)
  (occur cl-function-decl))

(require 'lisp-mode)

;;----------------------------------------------------------------------
;; auto mode list additions
;;----------------------------------------------------------------------

(setq
  auto-mode-alist (append '(("\\.cl$" . lisp-mode)
                            ("\\.lisp$" . lisp-mode)) auto-mode-alist ))

(defun profile/common-lisp ()
  (configure-for-programming 'cl-list-functions cl-lisp-name)

  (lisp-smart-parens-editing)

  (dwim-complete/for-buffer)

  (turn-on-dwim-tab 'lisp-indent-line))

(add-hook 'lisp-mode-hook 'profile/common-lisp t)

(provide 'profile/common-lisp)
