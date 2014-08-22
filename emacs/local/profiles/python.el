;;----------------------------------------------------------------------
;; python-programming
;;----------------------------------------------------------------------
(require 'dwim-tab)
(require 'generic-indent)
(require 'programming-generic)

;; builtin to emacs but stay with upstream master

(grail-load-package 'python-mode "bzr" "lp:python-mode")

(defconst profile/python-name "python")

(setq
  python-indent 2
  python-indent-offset 2)

(defconst python-function-regex "def")

(defun python-list-fn-signatures ()
  (interactive)
  (occur python-function-regex))

(defun profile/python-mode-setup ()
  (interactive)

  (programming-mode-generic 'python-list-fn-signatures)

  (buffer-ring/add profile/python-name)
  (buffer-ring/local-keybindings)

  (turn-on-dwim-tab)

  (grail-require profile/syntax-tools
    "python profile"
    "smart syntax"

    (profile/syntax-tools-mode-setup) ) )

(add-hook 'python-mode-hook 'profile/python-mode-setup t t)

;;
;; advanced ipython capability for repl
;;

(grail-load-package 'python-components-pdb "bzr" "lp:python-mode/components-python-mode")

(defconst profile/python-interpeter-exec "ipython")
(defconst profile/python-repl-name (borg-repl/repl-name profile/python-name) )

(defun profile/python-repl-buffer-name ()
  ;; has to be a function because it is not defined until run-python is called.
  (concat "*" python-shell-buffer-name "*"))

(provide 'profile/python)

