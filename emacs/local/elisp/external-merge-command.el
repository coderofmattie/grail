;;----------------------------------------------------------------------
;; external-merge-command
;;----------------------------------------------------------------------

(defun merge-write-ediff-buffer ()
  (let
    ((file ediff-merge-store-file))

    (set-buffer ediff-buffer-C)

    (write-region (point-min) (point-max) file)
      (message "Merge buffer saved in: %s" file)

      (set-buffer-modified-p nil)
      (sit-for 1)))

(defun merge-write-on-exit ()
  (add-hook 'ediff-quit-merge-hook 'ediff-write-merge-buffer t t))

(defun git-merge-with-ancestor ( local-file remote-file ancestor-file merge-file )
  (ediff-merge-files-with-ancestor
    local-file remote-file ancestor-file (list 'merge-write-on-exit) merge-file))

(provide 'external-merge-command)
