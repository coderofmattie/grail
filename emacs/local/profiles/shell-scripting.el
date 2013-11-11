;;----------------------------------------------------------------------
;; shell-script
;;----------------------------------------------------------------------

(setq advanced-bash-scripting-guide
  (grail-fetch-docs "advanced-bash-scripting-guide"
    (grail-define-installer "advanced-bash-scripting-guide" "tar:gz"
      "http://www.tldp.org/LDP/abs/abs-guide.html.tar.gz")
    1))

(setq advanced-bash-scripting-guide (concat "file://" advanced-bash-scripting-guide "/index.html"))

;; (code-documentation-browse-popup advanced-bash-scripting-guide)

(defconst shell-function-regex "function")

(defun shell-list-fn-signatures ()
  (interactive)
  (occur shell-function-regex))

(add-hook 'sh-mode-hook
  (lambda ()
    (configure-for-programming 'shell-list-fn-signatures "shell-mode")
    (setq sh-indentation 2)
    (procedural-smart-parens-editing) )
  t)
