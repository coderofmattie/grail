;;----------------------------------------------------------------------
;; cm-util.el
;;----------------------------------------------------------------------

;; this function was created because file-readable-p is strangely
;; akward in that it returns t instead of the path it was given which
;; neccesitates this silly wrapper. Consider sending this upstream as
;; a patch or add-on to file-readable-p"

(defun file-if-readable ( file )
  "if file is a readable path return file or nil"
  (if (file-readable-p file)
    file))

;; required for my patched em-unix, note: merged upstream, may collide
;; on a update.
(defun nil-blank-string ( string )
  "if a string is all blanks return nil, if there are non-blank characters return the string"
  (if (string-match "[^[:blank:]]" string ) string))

(provide 'cm-util)

