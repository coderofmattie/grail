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

(defun wc-run-rcs-command ( command args )
  (catch 'abort
    (let
      ((rcs-buffer (wc-command-buffer))
       (check-in-dir (file-name-directory (wc-local-file))))

      (unless (and check-in-dir (file-directory-p check-in-dir))
        (message "wc RCS abort! directory %s does not exist!")
        (throw 'abort))

      (let
        ((rcs-storage (concat check-in-dir "/RCS")))

        (unless (file-directory-p rcs-storage)
          (make-directory rcs-storage)) )

      (with-current-buffer rcs-buffer
        (apply 'start-process "working-copy-rcs" rcs-buffer command (cons "-t-wc-file" args)))

      (pop-to-buffer rcs-buffer nil t) )))

(defun wc-rcs-checkin ( log-message )
  (wc-run-rcs-command "ci" `("-u" ,(concat "-m" log-message) ,(wc-local-file)))
  (wc-run-rcs-command "co" `("-l" ,(wc-local-file))) )

(defun wc-local-commit ( log-message )
  (interactive "commit message: %s")
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

  (wc-rcs-checkin "first check-in"))

(provide 'working-copy)
