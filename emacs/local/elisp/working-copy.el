;;----------------------------------------------------------------------
;; working-copy
;; emulate local commit features and deal with perforce.
;;----------------------------------------------------------------------
(require 'file-utilities)
(require 'async-command-builders)
(require 'buffer-status)

(require 'subr-x)

(defvar wc-home-dir (concat (getenv "HOME") "/"))
(defvar wc-modeline-status "")

(defun wc-dir-for-file ( file )
  (concat wc-home-dir "code/working-copy/"
    (files-child-of-path wc-home-dir (file-name-directory file))) )

(defun wc-is-path-working-copy ( path )
  (file-readable-p (concat (wc-dir-for-file path) "/" (file-name-nondirectory path) ".local")) )

(defun wc-set-paths-for-buffer ()
  (make-local-variable 'wc-ancestor-file-path)
  (make-local-variable 'wc-local-file-path)
  (make-local-variable 'wc-upstream-file-path)
  (make-local-variable 'wc-merge-file-path)

  (let
    ((local-dir (wc-dir-for-file buffer-file-name))
      (filename  (file-name-nondirectory buffer-file-name)))

    (unless (grail-garuntee-dir-path local-dir)
      (message "wc: could not create or access directory: %s" local-dir) )

    (setq
      wc-ancestor-file-path (concat local-dir filename ".ancestor")
      wc-local-file-path    (concat local-dir filename ".local")
      wc-upstream-file-path (concat local-dir filename ".upstream")
      wc-merge-file-path    (concat local-dir filename ".merge")) ))

(defun wc-set-modeline-for-buffer ()
  (make-local-variable 'wc-modeline-status)
  (setq wc-modeline-status "|WC"))

(defun wc-ancestor-file ()
  wc-ancestor-file-path)

(defun wc-local-file ()
  wc-local-file-path)

(defun wc-upstream-file ()
  wc-upstream-file-path)

(defun wc-merge-file ()
  wc-merge-file-path)

(defun wc-working-copy-file-p ()
  (file-readable-p wc-local-file-path))

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
    (files-make-path-writable path))

  (wc-write-to-file path)

  (files-make-path-readonly path))

(defun wc-update-ancestor-file ()
  (interactive)
  (wc-update-protected-file (wc-ancestor-file)) )

(defun wc-update-upstream-file ()
  (interactive)
  (wc-update-protected-file (wc-upstream-file)) )

(defun wc-update-local-file ()
  (interactive)
  (wc-write-to-file (wc-local-file)))

(defun wc-rcs-command-buffer ()
  (get-buffer-create "*working-copy-rcs*"))

(defvar wc-wrappers-dir (concat grail-elisp-root "/wrappers/"))

(defun wc-popup-rcs-buffer ()
  (let
    ((old-buffer (current-buffer)))

    (pop-to-buffer (wc-rcs-command-buffer))
    (other-window 1)
    (switch-to-buffer old-buffer) ))

(defvar wc-rcs-last-exit-status nil)

(defun wc-rcs-command-callback ( status )
  (wc-popup-rcs-buffer)

  (if status
    (progn
      (message "rcs command failed. %s" status)
      (setq wc-rcs-last-exit-status nil))
    (progn
      (message "rcs command finished.")
      (setq wc-rcs-last-exit-status t))) )

(defun wc-rcs-command-builder ( &rest command-string )
  (async-build-basic "wc-rcs" (string-join command-string " ")
                     'wc-rcs-command-callback (wc-rcs-command-buffer)) )

(defun wc-rcs-command-chain-builder ( chain &rest builders )
  (async-build-basic "wc-rcs" (string-join command-string " ")
    'wc-rcs-command-callback (wc-rcs-command-buffer) chain))

(defun wc-rcs-command-builder-checkin ( path log )
  (wc-rcs-command-builder (concat wc-wrappers-dir "/rcs-checkin")
    path
    log))

(defun wc-rcs-command-builder-checkout ( path )
  (wc-rcs-command-builder "co" "-l" "-kb" path))

(defun wc-rcs-command-run ( constructed-command )
  (setq wc-rcs-last-exit-status nil)
  (apply 'grail-process-async-chain constructed-command))

(defun wc-rcs-init ( path )
  (wc-rcs-command-run
    (async-build-chained
      ("wc-rcs" (string-join
                  (list
                    (concat wc-wrappers-dir "/rcs-init")
                    path
                    "initialize rcs")
                  " ")
        'wc-rcs-command-callback (wc-rcs-command-buffer))

      ("wc-rcs" (string-join
                  (list
                    (concat wc-wrappers-dir "/rcs-checkin")
                    path
                    "first checkin")
                  " ")
        'wc-rcs-command-callback (wc-rcs-command-buffer)) )) )

(defun wc-rcs-checkout ( file )
  (wc-rcs-command-run
    (wc-rcs-command-builder-checkout file))

  (unless (wc-rcs-command-ok-p)
    (message "rcs command failure! for file %s" file))

  (unless (file-writable-p file)
    (message "rcs command completed for checkout but perms are botched: %s" file)) )

(defun wc-rcs-checkin ( file log-message )
  (wc-rcs-command-run
    (wc-rcs-command-builder-checkin file log-message))

  (when (wc-rcs-command-ok-p)
    (wc-rcs-checkout path)) )

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
     (this-file   (wc-local-file)))

    (with-current-buffer log-buffer
      (set (make-local-variable 'file-to-commit) this-file)
      (set (make-local-variable 'initiated-from-buffer) this-buffer)

      (local-set-key (kbd "C-c C-c") 'wc-commit-from-log-buffer))

    log-buffer))

(defun wc-checkout-local ()
  (interactive)
  (wc-rcs-checkout (wc-local-file)) )

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

(defun wc-primary-diff ( other-file other-name )
  (save-buffer)

  (catch 'same
    (when (wc-files-same-p buffer-file-name other-file)
      (message "wc: primary and %s copy are the same" other-name)
      (throw 'same t))

    (ediff-files other-file buffer-file-name)
    (message "A is %s B is primary" other-name) ))

(defun wc-diff-local ()
  (interactive)
  (wc-primary-diff (wc-local-file) "local") )

(defun wc-diff-upstream ()
  (interactive)
  (wc-primary-diff (wc-upstream-file) "upstream"))

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

    (wc-insert-from-file (wc-merge-file)) ))

(defun wc-key-bindings ()
  (local-set-key (kbd "C-c w s") 'wc-update-local-file)
  (local-set-key (kbd "C-c w c") 'wc-commit-local)

  (local-set-key (kbd "C-c w C") 'wc-commit)

  (local-set-key (kbd "C-c w u") 'wc-diff-upstream)
  (local-set-key (kbd "C-c w l") 'wc-diff-local) )

(defun wc-visit ()
  (interactive)

  (when (wc-is-path-working-copy (buffer-file-name))
    (buffer-status-add "working-copy enabled")

    (wc-set-paths-for-buffer)
    (wc-set-modeline-for-buffer)

    (wc-key-bindings) ))

(defun wc-init ()
  (interactive)

  (wc-set-paths-for-buffer)
  (wc-set-modeline-for-buffer)

  (wc-update-local-file)
  (wc-update-upstream-file)
  (wc-update-ancestor-file)

  (wc-rcs-init (wc-local-file)))

(defun wc-enable-globally ()
  (add-hook 'find-file-hook 'wc-visit t))

(provide 'working-copy)
