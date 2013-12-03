;;----------------------------------------------------------------------
;; remap-assoc-mode.el
;;----------------------------------------------------------------------

;; remap the mode associated with a file

(defun remap-assoc-mode-to ( old-mode new-mode )
  ;; it looks weird because it uses setcdr side effects
  (mapc
    (lambda (pair)
      (if (eq (cdr pair) old-mode)
        (setcdr pair new-mode)))
    (append auto-mode-alist interpreter-mode-alist)) )

(provide 'remap-assoc-mode)
