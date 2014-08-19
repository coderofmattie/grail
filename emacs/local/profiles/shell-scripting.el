;;----------------------------------------------------------------------
;; shell-script
;;----------------------------------------------------------------------
(require 'user-programming)
(require 'dwim-tab)

(require 'generic-indent)

(defconst shell-function-regex "function")
(defconst profile/shell-name "shell")

(defun shell-list-fn-signatures ()
  (interactive)
  (occur shell-function-regex))

(defun profile/shell-mode-setup ()
  (configure-for-programming 'shell-list-fn-signatures profile/shell-name)

  (buffer-ring/add profile/shell-name)
  (buffer-ring/local-keybindings)

  (setq
    sh-indentation 2
    sh-basic-offset 2)

    (local-set-key (kbd "<return>") 'hard-electric-newline)

  (grail-require profile/syntax-tools "shell scripting profile" "smart syntax"
    (profile/syntax-tools-mode-setup) )

  (turn-on-dwim-tab) )

(add-hook 'sh-mode-hook 'profile/shell-mode-setup t)

(provide 'profile/shell-scripting)
