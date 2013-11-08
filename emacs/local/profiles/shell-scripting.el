;;----------------------------------------------------------------------
;; shell-script
;;----------------------------------------------------------------------


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
