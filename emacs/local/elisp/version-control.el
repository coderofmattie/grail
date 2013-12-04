;;----------------------------------------------------------------------
;; version-control.el
;;----------------------------------------------------------------------
(require 'vc)
(require 'merging)

(defun ver-ctl-diff ()
  (interactive)
  (ediff-revision buffer-file-name) )

(defun ver-ctl-dir ()
  (interactive)
  (vc-dir (file-name-directory buffer-file-name)) )

(defun ver-ctl-revert ()
  (interactive)
  (if (yes-or-no-p "revert? y/n")
    (progn
      (vc-revert-file buffer-file-name)
      (revert-buffer nil t t))
    (message "nothing done.")) )

(defun ver-ctl-bindings ()
  (local-set-key (kbd "C-c r d") 'ver-ctl-diff)
  (local-set-key (kbd "C-c r v") 'ver-ctl-dir)
  (local-set-key (kbd "C-c r l") 'vc-print-log)

  (local-set-key (kbd "C-c r a") 'vc-register)
  (local-set-key (kbd "C-c r v") 'vc-next-action)

  (local-set-key (kbd "C-c r r") 'ver-ctl-revert)

  (local-set-key (kbd "C-c r m") 'vc-merge)
  (local-set-key (kbd "C-c r p") 'vc-pull))

(provide 'version-control)
