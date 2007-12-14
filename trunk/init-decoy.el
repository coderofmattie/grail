;;----------------------------------------------------------------------
;; .emacs.el
;;----------------------------------------------------------------------

;; the entire purpose of this file is to decoy any attempts by customize
;; to destroy my code.

(defun custom-save-variables ()
  (message "!CRITICAL! trapped attempt to modify emacs initialization file")

  (with-current-buffer "*Messages*"
    (backtrace)))

(load-file (concat (getenv "HOME") "/system/emacs/emacs.el"))
