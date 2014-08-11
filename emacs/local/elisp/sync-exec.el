;;----------------------------------------------------------------------
;; sync-exec.el
;;----------------------------------------------------------------------
(defun sync-exec-quote-args ( arg-list )
  (mapcar
    (lambda ( arg )
      (concat "'" arg "\'"))
    arg-list) )

(defun sync-exec-buffer ( buffer-name )
  (let
    ((exec-buffer (get-buffer-create buffer-name)))

    (with-current-buffer exec-buffer
      (term-mode))

    exec-buffer))

(defun sync-exec-clear ( buffer )
  (with-current-buffer buffer
    (erase-buffer)) )

(defun sync-exec ( exec-buffer command &rest args )
  (apply 'call-process command nil exec-buffer t args) )

(provide 'sync-exec)
