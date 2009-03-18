;;----------------------------------------------------------------------
;; cm-util.el
;;----------------------------------------------------------------------

(defun buffer-empty-p ( &optional buffer )
  "buffer-empty-p &optional buffer

   buffer-empty-p return t if the buffer is empty"
  (with-current-buffer (or (when (bufferp buffer) buffer)
                           (current-buffer))
    (not (> (point-max) (point-min)))) )

;; required for my patched em-unix, note: merged upstream, may collide
;; on a update.
(defun nil-blank-string ( string )
  "if a string is all blanks return nil, if there are non-blank characters return the string"
  (if (string-match "[^[:blank:]]" string ) string))

(provide 'cm-util)

