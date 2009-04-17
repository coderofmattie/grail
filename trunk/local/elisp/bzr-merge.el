(defun bzr-resolve-file ()
  (save-buffer)
  (message "would execute the resolve command"))

(defun bzr-merge-conflict ( file )
  (interactive "fMerge file with conflicts? ")

  (catch 'abort
    (let
      ((left  (concat file ".THIS"))
       (right (concat file ".OTHER"))
       (ancestor (concat file ".BASE")))

      (unless (file-exists-p left)  (throw abort 'nil))
      (unless (file-exists-p right) (throw abort 'nil))

      (if (file-exists-p ancestor)
        (ediff-merge-files-with-ancestor left right ancestor)
        (ediff-merge-files left right))
      t)) )

(provide 'bzr-merge)
