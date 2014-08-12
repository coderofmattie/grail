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

;; maybe possible to create a more terminal like buffer

;;    (with-current-buffer exec-buffer
;;      (term-mode))

    exec-buffer))

(defun sync-exec-clear ( buffer )
  (with-current-buffer buffer
    (erase-buffer)) )

(defun sync-exec-args ( dir exec-buffer command &rest args )
  (let
    ((default-directory dir))

    (apply 'call-process command nil exec-buffer t args) ))

(defun sync-exec-list ( dir exec-buffer command-list )
  (let
    ((default-directory (concat dir "/") ))

    (apply 'call-process (car command-list) nil exec-buffer t (cdr command-list)) ))

(provide 'sync-exec)
