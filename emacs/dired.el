;;----------------------------------------------------------------------
;; file-browser.el
;;----------------------------------------------------------------------
(require 'buffer-ring)

(require 'dired)

(defun tame-dired ()
  (buffer-ring/add "dired")
  (buffer-ring/local-keybindings))

(add-hook 'dired-mode-hook 'tame-dired)

(use-grail-profiles 0 "dir-tree")


