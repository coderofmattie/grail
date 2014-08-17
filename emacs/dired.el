;;----------------------------------------------------------------------
;; file-browser.el
;;----------------------------------------------------------------------
(require 'buffer-ring)

(require 'dired)

(defun tame-dired ()
  (configure-for-buffer-ring "dired") )

(add-hook 'dired-mode-hook 'tame-dired)

(use-grail-profiles 0 "dir-tree")


