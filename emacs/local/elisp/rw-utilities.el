;;----------------------------------------------------------------------
;; read/write handling
;;----------------------------------------------------------------------
(require 'file-utilities)

;; attempt to make paths writable. not normally how I do it but perforce
;; brain damage has brought me to this.

;; global key doesn't seem to have effect in view mode

(add-hook 'view-mode-hook
  (local-set-key (kbd "C-x C-q") 'rw-turn-off-buffer-read-only))

(defun rw-turn-off-buffer-read-only ()
  (interactive)
  (read-only-mode 0))

(defun rw-turn-on-buffer-read-only ()
  (interactive)
  (read-only-mode 1))

(defun rw-toggle-read-only ()
  (interactive)

  (if buffer-read-only
    (rw-turn-off-buffer-read-only)
    (rw-turn-on-buffer-read-only)) )

(defun rw-ask-if-make-writable ()
  (interactive)
  (unless (file-writable-p buffer-file-name)
    (if (eq 't (y-or-n-p "Read Only File: attempt to make writable? "))
      (progn
        (files-make-path-writable buffer-file-name)
        (rw-turn-off-buffer-read-only)
        (message "making file writable and turning off read-only") )
      nil)))

(add-hook 'before-save-hook 'rw-ask-if-make-writable t)

(global-set-key (kbd "C-x C-q") 'rw-toggle-read-only)

(provide 'rw-utilities)
