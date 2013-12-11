;;;----------------------------------------------------------------------
;; cm-string.el
;;;----------------------------------------------------------------------

(defun bracket-strings ( bracket list )
  (apply 'concat bracket (prefix-strings bracket list) bracket))

(defun prefix-strings ( prefix list )
  "prefix-strings PREFIX LIST

   transform LIST concatenating the strings with PREFIX."
  (mapcar
    (lambda ( string )
      (concat prefix string))
    list))

(defun string-join (prefix list)
  ;; This is analogous to the perl5 join function.
  ;; given a <prefix> and a <list> of strings join the
  ;; strings with <prefix> as a seperator between the
  ;; list values.
  ;;
  ;; The result is a single string value.
  (apply 'concat
    (car list)
    (if (cdr list) (prefix-strings prefix (cdr list))) ))

(defun string-join-args (prefix &rest args)
  (apply 'concat
    (car args)
    (if (cdr args) (prefix-strings prefix (cdr args))) ))

(defun string-strip-leading-whitespace ( string )
  (replace-regexp-in-string "^ *" "" string))

(provide 'cm-string)
