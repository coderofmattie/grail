;;----------------------------------------------------------------------
;; file-browser.el
;;----------------------------------------------------------------------
(require 'buffer-ring)

(require 'dired)

(defun tame-dired ()
  (configure-for-buffer-ring "dired") )

(add-hook 'dired-mode-hook 'tame-dired)

