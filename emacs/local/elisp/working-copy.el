;;----------------------------------------------------------------------
;; working-copy
;; emulate local commit features and deal with perforce.
;;----------------------------------------------------------------------

(defun wc-ancestor-file ()
  (concat buffer-file-name ".ancestor"))

(defun wc-local-file ()
  (concat buffer-file-name ".local"))

(defun wc-upstream-file ()
  (concat buffer-file-name ".local"))

(defun wc-write-to-file ( path )
  (save-excursion
    (let
      ((end-of-file (end-of-buffer)))

      (beginning-of-buffer)
      (write-region (point) end-of-file path) )))

(defun wc-update-ancestor-file ()
  (wc-write-to-file (wc-ancestor-file)))

(defun wc-update-local-file ()
  (wc-write-to-file (wc-local-file)))

(defun wc-update-upstream-file ()
  (wc-write-to-file (wc-upstream-file)))

(defun wc-working-copy-file-p ( path )
  (file-readable-p (concat path ".local")))

(defun wc-working-copy-buffer-p ()
  (file-readable-p (wc-local-file)))

(defun wc-init-for-buffer ()
  (wc-update-local-file)
  (wc-update-upstream-file)
  (wc-update-ancestor-file))

(provide 'working-copy)
