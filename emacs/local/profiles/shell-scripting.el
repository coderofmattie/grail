;;----------------------------------------------------------------------
;; shell-script
;;----------------------------------------------------------------------

(setq advanced-bash-scripting-guide
  (grail-fetch-docs "advanced-bash-scripting-guide"
    (grail-define-installer "advanced-bash-scripting-guide" "tar:gz"
      "http://www.tldp.org/LDP/abs/abs-guide.html.tar.gz")
    1))

(setq advanced-bash-scripting-guide (concat "file://" advanced-bash-scripting-guide "/index.html"))

(defconst shell-function-regex "function")

(defun shell-list-fn-signatures ()
  (interactive)
  (occur shell-function-regex))

(add-hook 'sh-mode-hook
  (lambda ()
    (configure-for-programming 'shell-list-fn-signatures "shell-mode")

    (code-documentation-setup "shell-docs" "shell-mode" advanced-bash-scripting-guide)

    (setq
      sh-indentation 2
      sh-basic-offset 2)

    (procedural-smart-parens-editing)
    (turn-on-dwim-tab))
  t)
