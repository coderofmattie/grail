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
  (insert-file-contents path nil nil nil t))

(defun wc-refresh-from-upstream ()
  (wc-insert-from-file (wc-upstream-file)))

(defun wc-refresh-from-local ()
  (wc-insert-from-file (wc-local-file)))

(defun wc-update-ancestor-file ()
  (wc-write-to-file (wc-ancestor-file)))

(defun wc-update-local-file ()
  (interactive)
  (wc-write-to-file (wc-local-file)))

(defun wc-update-upstream-file ()
  (interactive)
  (wc-write-to-file (wc-upstream-file)))

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
  (let
    ((log-buf (current-buffer)))

    (when (wc-rcs-checkin file-to-commit (wc-extract-log-from-buffer))
      (switch-to-buffer initiated-from-buffer)
      (other-window 1)
      (delete-other-windows)

      (kill-buffer log-buf)) ))

(defun wc-setup-log-buffer ()
  (let
    ((log-buffer (get-buffer-create "*rcs-log"))
     (this-buffer (current-buffer)))

    (with-current-buffer
      (make-variable-buffer-local 'file-to-commit)
      (make-variable-buffer-local 'initiated-from-buffer)

      (setq file-to-commit (wc-local-file))
      (setq initiated-from-buffer this-buffer)

      (local-set-key (kbd "C-c C-c") 'wc-commmit-from-log-buffer))

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

(defun wc-init ()
  (interactive)

  (wc-update-local-file)
  (wc-update-upstream-file)
  (wc-update-ancestor-file)

  (wc-rcs-checkin (wc-local-file) "first commit"))

(provide 'working-copy)
