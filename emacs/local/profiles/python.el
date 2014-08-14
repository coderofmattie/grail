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

(defun profile/python-cfg ()
  (interactive)

  (configure-for-navigation 'forward-word 'backward-word)
  (configure-for-programming 'python-list-fn-signatures "python-mode")

  (turn-on-dwim-tab)

  (procedural-smart-parens-editing)
  (setq sp-escape-char "\\") )

(add-hook 'python-mode-hook 'profile/python-cfg t t)

(defconst profile/python-interpeter-exec "ipython")

(defun profile/python-interpeter-buffer ()
  ;; has to be a function because it is not defined until run-python is called.
  (concat "*" python-shell-buffer-name "*"))

(defun profile/python-repl ( first-call )
  ;; this is all messed up because repl is messed up.

  (run-python profile/python-interpeter-exec)
  (lang-repl-mode-add "python-mode" profile/python-interpeter-buffer)

  ;; (when first-call
  ;;   (lang-repl-mode-add "python-mode" profile/python-interpeter-buffer))
  )

(lang-repl-mode-define "python-mode" 'profile/python-repl)

(provide 'grail/python)

