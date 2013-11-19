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

(defun wc-merge-file ()
  (concat buffer-file-name ".merge"))

(defun wc-working-copy-file-p ( path )
  (file-readable-p (concat path ".local")))

(defun wc-working-copy-buffer-p ()
  (file-readable-p (wc-local-file)))

(defun wc-write-to-file ( path )
  (save-excursion
    (let
      ((end-of-file (progn
                      (end-of-buffer)
                      (point))))

      (beginning-of-buffer)
      (write-region (point) end-of-file path) )))

(defun wc-insert-from-file ( path )
  (if (file-readable-p path)
    (insert-file-contents path nil nil nil t)
    (message "could not find file %s to load" path) ))

(defun wc-refresh-from-upstream ()
  (wc-insert-from-file (wc-upstream-file)))

(defun wc-refresh-from-local ()
  (wc-insert-from-file (wc-local-file)))

(defun wc-update-protected-file ( path )
  (when (file-readable-p path)
    (rw-make-path-writable path))

  (wc-write-to-file path)

  (rw-make-path-readonly path) )

(defun wc-update-ancestor-file ()
  (interactive)
  (wc-update-protected-file (wc-ancestor-file)) )

(defun wc-update-upstream-file ()
  (interactive)
  (wc-update-protected-file (wc-upstream-file)) )

(defun wc-update-local-file ()
  (interactive)
  (wc-write-to-file (wc-local-file)))

(defun wc-command-buffer ()
  (get-buffer-create "*working-copy-rcs*"))

(defvar wc-wrappers-dir (concat grail-elisp-root "/wrappers/"))

(defun wc-get-rcs-buffer ()
  (get-buffer-create "*wc rcs*"))

(defun wc-popup-rcs-buffer ()
  (let
    ((old-buffer (current-buffer)))

    (pop-to-buffer (wc-get-rcs-buffer) nil t)
    (other-window 1)
    (switch-to-buffer old-buffer) ))

(defun wc-build-rcs-command ( cmd next &rest args )
  (lexical-let
    ((full-args (cons cmd args))
     (bind-next next))

    `((lambda ()
        (start-process-shell-command "wc-rcs" (wc-get-rcs-buffer) ,(string-join " " full-args)) )

       (lambda ()
         (message "rcs command did not start!")
         (wc-popup-rcs-buffer))

       (lambda ()
         (message "rcs command returned error!")
         (wc-popup-rcs-buffer))

       (lambda ()
         (message "rcs command completed.")
         (wc-popup-rcs-buffer)
         t)
       ,bind-next) ))

(defun wc-run-rcs-command ( constructed-command )
  (wc-get-rcs-buffer)
  (apply 'grail-process-async-chain constructed-command))

(defun wc-rcs-checkout ( file )
  (wc-run-rcs-command
    (wc-build-rcs-command "co" nil "-l" file)) )

(defun wc-rcs-checkin ( file log-message )
  (lexical-let
    ((checkin-file file))

    (wc-run-rcs-command
      (wc-build-rcs-command
        (concat wc-wrappers-dir "rcs-ci")
        `(lambda ()
           (wc-run-rcs-command
             (wc-build-rcs-command "co" nil "-l" ,file)))
        file log-message)) ))

(defun wc-extract-log-from-buffer ()
  (let
    ((end-of-log (progn
                   (end-of-buffer)
                   (point)))
     (start-of-log
       (progn
         (beginning-of-buffer)
         (point))) )

    (buffer-substring start-of-log end-of-log) ))

(defun wc-commit-from-log-buffer ()
  (interactive)
  (let
    ((log-buf (current-buffer)))

    (when (wc-rcs-checkin file-to-commit (wc-extract-log-from-buffer))
      (switch-to-buffer initiated-from-buffer)
      (other-window 1)
      (delete-other-windows)

      (kill-buffer log-buf)) ))

(defun wc-setup-log-buffer ()
  (let
    ((log-buffer (get-buffer-create "*wc-log*"))
     (this-buffer (current-buffer))
     (this-file  (wc-local-file)) )

    (with-current-buffer log-buffer
      (set (make-local-variable 'file-to-commit) this-file)
      (set (make-local-variable 'initiated-from-buffer) this-buffer)

      (local-set-key (kbd "C-c C-c") 'wc-commit-from-log-buffer))

    log-buffer))

(defun wc-commit-local ()
  (interactive)

  (let
    ((log-buffer (wc-setup-log-buffer)))

    (pop-to-buffer log-buffer nil t)
    (message "please enter a commit message followed by C-c C-c") ))

(defun wc-commit ()
  (interactive)

  (wc-update-local-file)
  (wc-commit-local) )

(defun wc-files-same-p ( file-a file-b )
  (let
    ((exit-status nil))

    (with-temp-buffer
      (setq exit-status
        (shell-command (format "cmp %s %s" file-a file-b) (current-buffer) (current-buffer)) ))

    (if (equal 0 exit-status)
      t
      nil) ))

(defun wc-diff ( other-file other )
  (save-buffer)

  (catch 'same
    (when (wc-files-same-p buffer-file-name other-file)
      (message "wc: primary and %s copy are the same" other)
      (throw 'same t))

    (ediff-files buffer-file-name other-file)
    (message "A is primary , B is %s" other) ))

(defun wc-diff-local ()
  (interactive)
  (wc-diff (wc-local-file) "local") )

(defun wc-diff-upstream ()
  (interactive)
  (wc-diff (wc-upstream-file) "upstream"))

(defun wc-start-merge ()
  (interactive)

  (when (file-readable-p (wc-merge-file))
    (delete-file (wc-merge-file)) )

  (ediff-merge-files-with-ancestor
    (wc-upstream-file) (wc-local-file) (wc-ancestor-file)
    nil (wc-merge-file)) )

(defun wc-finish-merge ()
  (interactive)

  (catch 'abort
    (unless (file-readable-p (wc-merge-file))
      (message "there is no merge in progress to finish.")
      (throw 'abort t))

    (wc-insert-from-file (wc-merge-file))
    (file-delete (wc-merge-file)) ))

(defun wc-init ()
  (interactive)

  (wc-update-local-file)
  (wc-update-upstream-file)
  (wc-update-ancestor-file)

  (wc-rcs-checkin (wc-local-file) "first commit"))

(provide 'working-copy)
