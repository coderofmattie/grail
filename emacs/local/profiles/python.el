;;----------------------------------------------------------------------
;; python-programming
;;----------------------------------------------------------------------

;; builtin to emacs but stay with upstream master
(grail-load-package 'python-mode "bzr" "lp:python-mode")

(grail-load 'python-components-pdb "bzr" "lp:python-mode/components-python-mode")

(require 'generic-indent)

(setq
  python-indent 2
  python-indent-offset 2)

(defconst python-function-regex "def")

(defun python-list-fn-signatures ()
  (interactive)
  (occur python-function-regex))

(defun profile/python-cfg ()
  (interactive)

  (configure-for-navigation 'forward-word 'backward-word)
  (configure-for-programming 'python-list-fn-signatures "python-mode")

  (turn-on-dwim-tab)

  (grail-requires profile/syntax-tools "python profile" "smart syntax"
    (profile/syntax-tools-setup) ) )

(add-hook 'python-mode-hook 'profile/python-cfg t t)

(defconst profile/python-interpeter-exec "ipython")

(defun profile/python-interpeter-buffer ()
  ;; has to be a function because it is not defined until run-python is called.
  (concat "*" python-shell-buffer-name "*"))

(provide 'profile/python)

