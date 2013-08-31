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

;;----------------------------------------------------------------------
;; function-arity
;;----------------------------------------------------------------------

;; copied from the page:
;; http://www.mail-archive.com/help-gnu-emacs@gnu.org/msg00056.html
;; Author: Kevin Rodgers, kevin.d.rodgers@gmail.com

(require 'help-fns)

(defun lambda-arity (function)
  "Return minimum and maximum number of args allowed for FUNCTION.
FUNCTION must be a symbol whose function binding is a lambda expression
or a macro.
The returned value is a pair (MIN . MAX).  MIN is the minimum number
of args.  MAX is the maximum number or the symbol `many', for a lambda
or macro with `&rest' args."
  (let* ((arglist (help-function-arglist function))
         (optional-arglist (memq '&optional arglist))
         (rest-arglist (memq '&rest arglist)))
    (cons (- (length arglist)
             (cond (optional-arglist (length optional-arglist))
                   (rest-arglist (length rest-arglist))
                   (t 0)))
          (cond (rest-arglist 'many)
                (optional-arglist (+ (length arglist)
                                     (length optional-arglist)
                                     -1))
                (t (length arglist))))))

(defun function-arity ( function )
  (if (subrp function)
    (subr-arity function)
    (lambda-arity function)))

(provide 'cm-lisp)

