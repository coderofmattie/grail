;;----------------------------------------------------------------------
;; macros - a library of routines to deal with macro problems
;;
;; description:
;;
;; there are many issues with how emacs implements macros such as
;; capture issues. This library helps deal with such problems
;;----------------------------------------------------------------------

(defun macros-anon-symbol ()
  (make-symbol (format "anonymous-symbol-%s" (random 100000))))

(defun macros-bind-value ( value )
  (let
    ((new-sym (make-symbol (format "anonymous-symbol-%s" (random 100000)) )))
    (set new-sym value)

    new-sym))

(defun macros-symbol-value-soft ( symbol )
  (let
    ((symbol-value (cons nil nil)))

    (condition-case nil
      (let
        (( value (symbol-value symbol) ))

        (setcdr symbol-value value)
        (setcar symbol-value t)

        symbol-value)
      (error symbol-value)) ))

;; (setq foo "crappy")

;; (setq bar "shitty")

;; (princ (macros-symbol-value-soft 'foo) )
;; (princ  (macros-symbol-value-soft (macros-bind-value "shitty") ))

(defun macros-symbol-try-eval ( symbol )
  (let
    ((symbol-value (macros-symbol-value-soft symbol)))

    (if (car symbol-value)
      (cdr symbol-value)
      symbol) ))

(defun macros-symbol-value-recursive ( list )
  (mapcar
    (lambda ( element )
      (cond
        ((symbolp element) (macros-symbol-try-eval element))
        ((listp element) (macros-symbol-value-recursive element))
        (t element) ))
    list))

;; (eval (macros-symbol-value-recursive `(message "%s %s" foo bar)))
;; (macros-symbol-value-recursive `(message "%s %s" foo bar))

(macros-symbol-value-recursive `(message "%s %s" foo (lambda () "foo")))

(defmacro macros-insert-value ( &rest body )
  `,(car body))

(macros-insert-value "foo")

(defun macros-bind-eval ( value )
  (let
    ((wrapped-lambda
       (lexical-let
         ((wrapped-value))

         (lambda () wrapped-value)) ))

    (let
      ((bound-lambda (macros-anon-symbol) ))

      (fset bound-lambda wrapped-lambda)
      bound-lambda) ))

(provide 'macros)
