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

(defun pforce-is-subdir-of ( root-path child-path )
  (catch 'done
    (let
      ((root-dirs  (split-string root-path  "[/]"))
       (child-dirs (split-string child-path "[/]"))
       (is-child nil))

      (while t
        (let
          ((next-root (car root-dirs))
           (next-child (car child-dirs)))

          (setq root-dirs (cdr root-dirs))
          (setq child-dirs (cdr child-dirs))

          (unless (and next-root next-child)
            (throw 'done nil))

          (unless (string-equal next-root next-child)
            (throw 'done nil))

          (when (eq nil root-dirs)
            (setq is-child t)
            (throw 'done t)) )) )
    is-child))

(defun perforce-is-perforce-tree ( path )
  (let
    ((query-path (if (file-directory-p path) path (file-name-directory path))))

    (catch 'is-pforce
      (mapc (lambda ( perforce-tree )
              (when (pforce-is-subdir-of perforce-tree query-path)
                (throw 'is-pforce t)))
        perforce-trees)

      nil)))

(defun perforce-convert-to-working-copy-hook ()
  (interactive)

  (when (and (perforce-is-perforce-tree buffer-file-name) (not (wc-working-copy-buffer-p)))
    (when (eq 't (y-or-n-p "P4 locked file convert -> working copy? "))
      (wc-init-for-buffer)
      (rw-make-path-writable buffer-file-name)
      (message "converted this buffer from perforce -> working copy") )))

(defun perforce-activate-with-working-copy ()
  (add-hook 'before-save-hook 'perforce-convert-to-working-copy-hook))

(provide 'perforce-utilities)
