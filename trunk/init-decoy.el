;;----------------------------------------------------------------------
;; .emacs.el
;;----------------------------------------------------------------------

;; Any of the functions custom- are subject to rewrite by customize.
;; This is not optimal for me as I am still migrating a few things
;; over to setq.

;; When this work is finished the better approach would be to put
;; the customization settings in .emacs.d . Until that is complete
;; this file protects my config from rewrites by essentially disabling
;; any customization modifications.

(setq custom-file "/dev/null")

(defun backtrace-on-custom ()
  (message "!CRITICAL! trapped attempt to modify emacs initialization file")

  (with-current-buffer "*Messages*"
    (backtrace)))

;; This is a good canidate for defadvise.

(defun custom-save-customized ()
  (backtrace-on-custom))

(defun custom-save-all ()
  (backtrace-on-custom))

(defun custom-save-faces ()
  (backtrace-on-custom))

(defun custom-save-variables ()
  (backtrace-on-custom))

;; load the real emacs file.
(load-file (concat (getenv "HOME") "/system/emacs/emacs.el"))
