;;----------------------------------------------------------------------
;; dealing with perforce issues.
;;----------------------------------------------------------------------
(require 'rw-utilities)
(require 'working-copy)

(defvar perforce-trees nil)

(defun perforce-in-use-p ()
  (if perforce-trees
    t
    nil))

(defun pforce-add-tree ( tree )
  (setq perforce-trees (cons tree perforce-trees)) )

(defun perforce-is-perforce-tree ( path )
  (let
    ((query-path (if (file-directory-p path) path (file-name-directory path))))

    (catch 'is-pforce
      (mapc (lambda ( perforce-tree )
              (when (files-child-of-path perforce-tree query-path)
                (throw 'is-pforce t)))
        perforce-trees)
      nil)))

(defun perforce-convert-to-working-copy-hook ()
  (interactive)

  (when (and (perforce-is-perforce-tree buffer-file-name) (not (wc-working-copy-buffer-p)))
    (when (eq 't (y-or-n-p "P4 locked file convert -> working copy? "))
      (wc-init-for-buffer)
      (files-make-path-writable buffer-file-name)
      (message "converted this buffer from perforce -> working copy") )))

(defun perforce-activate-with-working-copy ()
  (add-hook 'before-save-hook 'perforce-convert-to-working-copy-hook))

(provide 'perforce-utilities)
