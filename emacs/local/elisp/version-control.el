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

(defun ver-ctl-bindings ()
  (local-set-key (kbd "C-c r d") 'ver-ctl-diff)
  (local-set-key (kbd "C-c r v") 'ver-ctl-dir) )

(provide 'version-control)
