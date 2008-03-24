;;----------------------------------------------------------------------
;; cm-lisp.el
;;----------------------------------------------------------------------

(defun eqn ( a b )
  "eqn A B

   A and B are equal if their symbol names are a string match.
  "
  (string-equal (symbol-name a) (symbol-name b)))

(defun bind-eval-lambda ( name sexp )
  "bind-eval-lambda NAME SEXP
   bind the function of a un-interned symbol named NAME to an evaluation
   of a SEXP."
  (let
    ;; this would be cooler if it used one of the unique algorithms.
    ((anon-func (make-symbol name)))
    (fset anon-func (eval sexp))
    anon-func))

(provide 'cm-lisp)

