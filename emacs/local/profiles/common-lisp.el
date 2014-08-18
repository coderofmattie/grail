;;----------------------------------------------------------------------
;; common lisp
;;----------------------------------------------------------------------
(require 'dwim-tab)
(require 'user-programming)

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

(defun profile/cl-mode-setup ()
  (configure-for-programming 'cl-list-functions cl-lisp-name)

  (grail-require profile/syntax-tools
    (profile/syntax-tools-mode-setup)
    (profile/syntax-tools-lisp) )

  (dwim-complete/for-buffer)

  (turn-on-dwim-tab 'lisp-indent-line) )

(add-hook 'lisp-mode-hook 'profile/cl-mode-setup t)

(provide 'profile/common-lisp)
