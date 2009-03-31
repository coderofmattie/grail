;;----------------------------------------------------------------------
;; unterminated lists experiments.
;;----------------------------------------------------------------------

(defun terminate-sequence ( &rest args )
  "terminate sequence takes a all types concatenates into a list properly handling unterminated sequences"
  (lexical-let
    ((terminated nil))

    (dolist (arg (reverse args))
      (if (and (consp arg) (not (eq 'quote (car arg))))
        (lexical-let
          ((reverse-stack nil)
            (sequence arg))

          (while (consp sequence)
            (push (car sequence) reverse-stack)
            (setq sequence (cdr sequence)))

          (if sequence (push sequence reverse-stack))
          (setq terminated (append (reverse reverse-stack) terminated)))

        (setq terminated (cons arg terminated)) ))
    terminated))

(defun terminated-list-p ( list )
  "return true only if the list is nil terminated"
  (if (consp list)
    (lexical-let
      ((element (cdr list)))

      (while (consp element)
        (setq element (cdr element)))

      (eq nil element)) ))
