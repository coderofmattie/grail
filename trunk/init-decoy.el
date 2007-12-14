;;----------------------------------------------------------------------
;; .emacs.el
;;----------------------------------------------------------------------

;; the entire purpose of this file is to decoy any attempts by customize
;; to destroy my code.

(defun backtrace-on-custom ()
  (message "!CRITICAL! trapped attempt to modify emacs initialization file")

  (with-current-buffer "*Messages*"
    (backtrace)))

(defun custom-save-all ()
  (backtrace-on-custom))

(defun custom-save-faces ()
  (backtrace-on-custom))

(defun custom-save-variables ()
  (backtrace-on-custom))

(load-file (concat (getenv "HOME") "/system/emacs/emacs.el"))
