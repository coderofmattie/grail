;;----------------------------------------------------------------------
;; closure.el
;;
;; pseudo closures to save and re-enter an execution environment.
;;----------------------------------------------------------------------
(defconst closure-release-version "0.0.2"
  "the release number of closure.el")

;; These functions are experimental prototypes of elisp language features.
;; A robust implementation would be at the C level.

;; These functions such as save-lexical-closure and use-dynamic-closure
;; allow the execution environment of a function to be saved, and used
;; by another function.

;; The scope then becomes data shared between a related set of functions.

;;----------------------------------------------------------------------
;; definition and creation
;;----------------------------------------------------------------------

(defmacro closure-define ( symbol &rest definitions )
  "closure-define SYMBOL &DEF

   This macro form binds the definition list DEF to SYMBOL. The definition
   is used to later instantiate a closure with closure-create.

   the symbol itself returned.

   (closure-define foo
      (example   nil)
      (value     \"foo\"))
  "

  (set symbol
    (mapcar
      (lambda ( def )
        (cons (car def) (cadr def)))
      definitions))
  symbol)

(defvar closure-objarray-bucket-tuning 13
  "objarray creation requires a tuning value.")

(defun closure-copy ( closure )
  "copy CLOSURE an objarray so that the values are not shared unlike copy-sequence."
  (lexical-let
    ((copy (make-vector closure-objarray-bucket-tuning 0)))

    (mapatoms
      (lambda ( s )
        (lexical-let
          ((name (symbol-name s)))
          (set (intern name copy) (symbol-value (intern name closure))))) closure)
    copy))

(defun closure-create ( definition )
  "closure-create DEF

   Create a closure which is an objarray initialized by the definition DEF
   from closure-define.

   (closure-create (closure-define ....))
  "
  (lexical-let
    ((table (make-vector closure-objarray-bucket-tuning 0)))

    (mapc (lambda ( pair )
            (set (intern (symbol-name (car pair)) table) (eval (cdr pair)))) definition)
    table))

;;----------------------------------------------------------------------
;; binding
;;----------------------------------------------------------------------

(defun closure-bind-scope ( closure body )
  "closure-bind-scope CLOSURE BODY

   Traverse the sexp tree depth first pre-binding any symbol found in closure.
  "

  ;; Note: might be better to just use a cl low level library function.

  ;; It is a standard recursive descent transform of the body.  If the
  ;; current symbol "atom" in the descent is intern'd or found in the
  ;; closure objarray the intern is injected. Otherwise the symbol is
  ;; preserved as is.

  (if (consp body)
    (lexical-let
      ((atom (car body)))

      (cons
        (if (listp atom)
          (closure-bind-scope closure atom)

          (if (symbolp atom)
            (or
              (intern-soft (symbol-name atom) closure)
              atom)
            atom))

        (closure-bind-scope closure (cdr body))))
    body))

(defmacro save-lexical-closure ( closure &rest body )
  "a persistent lexical binding. The objarray CLOSURE appears lexically
   scoped in that a recursive traversal binds symbols of equal name
   in CLOSURE. altering these pre-bound symbols with setq changes the
   value in CLOSURE allowing the values to persist beyond the form in
   objarray CLOSURE.

   Currently this is a experimental hack so it incurs the cost
   of a recursive pre-bind in addition to eval each time evaluated."
  (declare (debug (symbolp body)))

  ;; This is mildly evil. The body is quoted into the result of the
  ;; macro.  This defers closure-bind-scope until eval *after* the
  ;; macro expansion.

  ;; This has to be done so that the binding happens at evaluation
  ;; time when the closure exists instead of compile time.

  `(eval (closure-bind-scope ,closure ',(if (eq 'lambda (car body))
                                          body
                                          (cons 'progn body)))))

(defun closure-let-binding ( s closure )
  "closure-let-binding s closure

   create a let pair (sym value). a un-intern'd symbol must
   be created. I do so with the read function. the value is
   retrieved from the closure.

   This binding of the closure is read-only.
  "
  `(,(read (symbol-name s)) ,(closure-symbol s closure)))

(defmacro use-dynamic-closure ( with-def &rest body )
  "use-dynamic-closure (DEF CLOSURE) BODY

   create a dynamic scope from the closure specified as
   DEF the closure definition, and the instance CLOSURE.

   The values from the closure can be used and modified
   however they are copies so the closure itself is not
   modified.
   "
  (declare (debug (form body)))
  (lexical-let
    ((definition    (eval (car with-def)))
     (closure       (eval (cadr with-def)))
     (bindings      nil))

    `(let
       ,(progn
          (mapc
            (lambda ( def )
              (push (closure-let-binding (car def) closure) bindings))
            definition)
          bindings)
       ,@body)))

(defmacro use-dynamic-closure-with ( with-def let-spec &rest body )
  "use-dynamic-closure-with (DEF CLOSURE) (LET) BODY

   This form is the same as use-dynamic-closure adding let pairs
   exclusive to the body.

   (use-dynamic-closure-with
     (closure-def my-closure)
     ((foo value)
      (bar value))

       body...)
  "
  (declare (debug (form form body)))

  (lexical-let
    ((definition    (eval (car with-def)))
     (closure       (eval (cadr with-def)))
     (bindings      nil))

    `(let
       ,(progn
          (mapc
            (lambda ( def )
              (push (closure-let-binding (car def) closure) bindings))
            definition)
          (append
            let-spec
            bindings))
       ,@body)))

;;----------------------------------------------------------------------
;; direct access
;;----------------------------------------------------------------------

(defun closure-value ( symbol closure )
  "closure-value SYMBOL CLOSURE

   return the value of SYMBOL in CLOSURE.
  "
  (symbol-value (intern (symbol-name symbol) closure)))

(defun closure-symbol ( symbol closure )
  "closure-symbol SYBMOL CLOSURE

   return SYMBOL from closure.
  "
  (intern (symbol-name symbol) closure))

;;----------------------------------------------------------------------
;; utilities
;;----------------------------------------------------------------------

(defun pp-closure ( closure )
  "pretty print a closure returning a string."
  (lexical-let
    ((strings nil))

    (mapatoms
      (lambda ( s )
        (push (format "symbol: %s = %s\n"
                (symbol-name s)
                (pp-to-string (symbol-value (intern (symbol-name s) closure)))) strings)) closure)
    (apply 'concat strings)))

(defun pp-closure-filtered ( filter closure )
  "pretty print a closure returning a string with filtering."
  (lexical-let
    ((strings nil))

    (mapatoms
      (lambda ( s )
        (lexical-let*
          ((name (symbol-name s))
           (value (symbol-value s)))

          (unless (funcall filter value)
            (push (format "symbol: %s = %s\n"
                    name
                    (pp-to-string value)) strings)) )) closure)
    (apply 'concat strings)))

(provide 'closure)

;;; closure.el ends here
