;;----------------------------------------------------------------------
;; cm-util.el
;;----------------------------------------------------------------------

(defun buffer-empty-p ( &optional buffer )
  "buffer-empty-p &optional buffer

   buffer-empty-p return t if the buffer is empty"
  (with-current-buffer (or (when (bufferp buffer) buffer)
                           (current-buffer))
    (not (> (point-max) (point-min)))) )

;; this function was created because file-readable-p is strangely
;; akward in that it returns t instead of the path it was given which
;; neccesitates this silly wrapper. Consider sending this upstream as
;; a patch or add-on to file-readable-p"

(defun file-path-if-readable ( file )
  "return the path if the file is readable, otherwise nil"
  (if (file-readable-p file)
    file))

;; required for my patched em-unix, note: merged upstream, may collide
;; on a update.
(defun nil-blank-string ( string )
  "if a string is all blanks return nil, if there are non-blank characters return the string"
  (if (string-match "[^[:blank:]]" string ) string))

(defun delete-trailing-path-separators ( path )
  "delete-trailing-path-separators

   Delete any trailing separators from the path, returning the modified path.
  "
  (let
    ((i (- (length path) 1)))

    (while (and (> i 0) (char-equal ?/ (elt path i)))
      (setq i (- i 1)))

    (substring path 0 (+ i 1))))


(provide 'cm-util)

