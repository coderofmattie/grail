;;----------------------------------------------------------------------
;; working-copy
;; emulate local commit features and deal with perforce.
;;----------------------------------------------------------------------

(defun wc-ancestor-file ()
  (concat buffer-file-name ".ancestor"))

(defun wc-local-file ()
  (concat buffer-file-name ".local"))

(defun wc-upstream-file ()
  (concat buffer-file-name ".upstream"))

(defun wc-write-to-file ( path )
  (save-excursion
    (let
      ((end-of-file (progn
                      (end-of-buffer)
                      (point))))

      (beginning-of-buffer)
      (write-region (point) end-of-file path) )))

(defun wc-update-ancestor-file ()
  (wc-write-to-file (wc-ancestor-file)))

(defun wc-update-local-file ()
  (interactive)
  (wc-write-to-file (wc-local-file)))

(defun wc-update-upstream-file ()
  (interactive)
  (wc-write-to-file (wc-upstream-file)))

(defun wc-working-copy-file-p ( path )
  (file-readable-p (concat path ".local")))

(defun wc-working-copy-buffer-p ()
  (file-readable-p (wc-local-file)))

(defun wc-command-buffer ()
  (get-buffer-create "*working-copy-rcs*"))

(defvar wc-wrappers-dir (concat grail-elisp-root "/wrappers/"))

(defun wc-run-rcs-command ( command args )
  (catch 'abort
    (let
      ((rcs-buffer (get-buffer-create "*wc rcs*"))
       (old-buffer (current-buffer)))

      (save-excursion
        (with-current-buffer rcs-buffer
          (apply 'start-process "working-copy-rcs" rcs-buffer command args))

        (pop-to-buffer rcs-buffer nil t)
        (other-window 1) )) ))

(defun wc-rcs-start-for-buffer ()
  (let
    ((check-in-dir (file-name-directory (wc-local-file))))

    (unless (and check-in-dir (file-directory-p check-in-dir))
      (let
        ((rcs-storage (concat check-in-dir "/RCS")))

        (unless (file-directory-p rcs-storage)
          (make-directory rcs-storage)) )))

  (wc-rcs-checkin "first commit"))

(defun wc-rcs-checkout-only ()
  (wc-run-rcs-command "co" `("-l" ,(wc-local-file))) )

(defun wc-rcs-checkin ( log-message )
  (wc-run-rcs-command (concat wc-wrappers-dir "rcs-ci") `( ,(wc-local-file) ,log-message))
  (wc-rcs-checkout-only))

(defun wc-local-commit ( log-message )
  (catch 'abort
    (unless log-message
      (message "you did not supply a commit message")
      (throw 'abort 't))

    (wc-run-rcs-command "trucate" `("-s0" ,(wc-local-file)))

    (wc-update-local-file)
    (wc-rcs-checkin log-message) ))

(defun wc-restore-from-bad-commit ()
  (interactive)
  (wc-update-local-file)
  (message "local file restored"))

(defun wc-init-for-buffer ()
  (interactive)

  (wc-update-local-file)
  (wc-update-upstream-file)
  (wc-update-ancestor-file)

  (wc-rcs-start-for-buffer))

(provide 'working-copy)
