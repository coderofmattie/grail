;;----------------------------------------------------------------------
;; python-programming
;;----------------------------------------------------------------------

;; builtin to emacs but stay with upstream master
(grail-load 'python-mode (grail-define-installer "python-mode"
                          "bzr"
                          "lp:python-mode"))

(grail-load 'python-components-pdb (grail-define-installer "python-components"
                                    "bzr"
                                    "lp:python-mode/components-python-mode"))

(require 'generic-indent)

(setq
  python-indent 2
  python-indent-offset 2)

(defconst python-function-regex "def")

(defun python-list-fn-signatures ()
  (interactive)
  (occur python-function-regex))

(defun programming-python-cfg ()
  (interactive)

  (configure-for-navigation 'forward-word 'backward-word)
  (configure-for-programming 'python-list-fn-signatures "python-mode")

  (turn-on-dwim-tab)

  (procedural-smart-parens-editing)
  (setq sp-escape-char "\\") )

(add-hook 'python-mode-hook 'programming-python-cfg t)
