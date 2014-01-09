;;;----------------------------------------------------------------------
;; cm-string.el
;;;----------------------------------------------------------------------

(defun cm-bracket-strings ( bracket list )
  (apply 'concat bracket (prefix-strings bracket list) bracket))

(defun cm-prefix-strings ( prefix list )
  "prefix-strings PREFIX LIST

   transform LIST concatenating the strings with PREFIX."
  (mapcar
    (lambda ( string )
      (concat prefix string))
    list))

(defun cm-string-join (prefix &rest args )
  (when (equal (length args) 1)
    (setq args (car args)))

  (apply 'concat
    (car args)
    (if (cdr args) (cm-prefix-strings prefix (cdr args))) ))

(defun cm-string-join-args (prefix &rest args)
  (apply 'concat
    (car args)
    (if (cdr args) (cm-prefix-strings prefix (cdr args))) ))

(defun string-strip-leading-whitespace ( string )
  (replace-regexp-in-string "^ *" "" string))

(provide 'cm-string)
