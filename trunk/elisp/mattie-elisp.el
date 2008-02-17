;;----------------------------------------------------------------------
;; mattie-elisp.el
;; Primary Author: Mike Mattie
;; Copyright (C) 2008 Mike Mattie
;; Description: basic elisp programming tools.
;;----------------------------------------------------------------------

;; required for my patched em-unix, note: merged upstream, may collide
;; on a update.
(defun nil-blank-string ( string )
  "if a string is all blanks return nil, if there are non-blank characters return the string"
  (if (string-match "[^[:blank:]]" string ) string))

(defun make-anon-func ( name sexp )
  "bind an un-evaluated anonymous function to an un-interned symbol"
  (let
    ;; this would be cooler if it used one of the unique algorithms.
    ((anon-func (make-symbol name)))
    (fset anon-func (eval sexp))
    anon-func))

;; much like easy-mmode-define-keymap macro but with a little more
;; juice doing the defvar part as well.

(defmacro def-sparse-map ( symbol docstring &rest keys )
  "make it easy to define a keymap give the symbol, a docstring, followed by
   the usual (key 'symbol) lists."
  `(defvar ,symbol
     (let
       ((map (make-sparse-keymap)))
       ,@(mapcar (lambda (binding)
                   (list 'define-key 'map (car binding) (cadr binding))) keys)
       map)
     ,docstring))

(defun mode-overlay-at-point-p ( mode-symbol )
  "determine if the point is in a flyspell overlay. given a overlay list
   which may be nil, translate via predicate into boolean values which
   are then evaluated by or."
  (interactive)
  (let
    ((overlay-list (overlays-at (point))))

    (when overlay-list
      (eval (cons 'or
              (mapcar
                (lambda ( overlay )
                  (if (overlay-get overlay mode-symbol) t)) overlay-list)))) ))

;; I really like this implementation, would map-filter-nil benefit from
;; using consp ?
(defun list-filter-nil ( list )
  "Filter nil symbols from a list"
  (if (consp list)
    (lexical-let
      ((head (car list)))

      (if (eq head 'nil)
        (list-filter-nil (cdr list))
        (cons head (list-filter-nil (cdr list)))
        ))
    nil))

;; I really like this implementation, would map-filter-nil benefit from
;; using consp ?
(defun seq-filter-nil ( &rest list-seq )
  "Filter nil symbols from a sequence."
  (list-filter-nil list-seq))

;; define-error originated in XEmacs. This implementation shares the
;; same name, but not the interface. I need to clone or copy the
;; XEmacs version.

(defmacro define-error ( symbol message &rest isa-list )
  "define a error symbol with a isa list and a error message"
  `(progn
     (put ',symbol
       'error-conditions (append '(error ,symbol) ',isa-list))
     (put ',symbol 'error-message ,message) ))

;;----------------------------------------------------------------------
;; closures
;;----------------------------------------------------------------------

;; these are experimental prototypes. The final versions need to be
;; implemented at the C level.

(defun scope-bind-closure ( closure body )
  "traverse the tree depth first pre-binding any symbol found in closure."

  (if (consp body)
    (lexical-let
      ((atom (car body)))

      (cons
        (if (listp atom)
          (scope-bind-closure closure atom)

          (if (symbolp atom)
            (or
              (intern-soft (symbol-name atom) closure)
              atom)
            atom))

        (scope-bind-closure closure (cdr body))))
    body))

(defmacro save-lexical-closure ( closure &rest body )
  "a persistent lexical binding. The objarray CLOSURE appears lexically
   scoped in that a recursive traversal binds symbols of equal name
   in CLOSURE. altering these pre-bound symbols with setq changes the
   value in CLOSURE allowing the values to persist beyond the form in
   objarray CLOSURE.

   Currently this is a experimental hack so it incurs the cost
   of a recursive pre-bind in addition to eval each time evaluated."
;; New theory for why this doesn't? work. scope-bind-closure mangled
;; the code erroneously.
  (declare (debug (symbolp body)))
  `(eval (scope-bind-closure ,closure ',(cons 'progn body))))

(defmacro use-dynamic-closure ( closure &rest body )
  "use a saved closure as a dynamic scope with private copy."
  (declare (debug (symbolp body)))
  `(let
     ,(lexical-let
        ((bindings nil))
        (mapatoms
          (lambda ( s )
            (push `(,(read (symbol-name s))
                     (symbol-value (intern ,(symbol-name s) ,closure)))
              bindings))
          (eval closure))
        bindings)
     ,@body))

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

(defun copy-closure ( closure )
  "copy CLOSURE an objarray so that the values are not shared unlike copy-sequence."
  (lexical-let
    ((copy (make-vector (length closure) 0)))

    (mapatoms
      (lambda ( s )
        (lexical-let
          ((name (symbol-name s)))
          (set (intern name copy) (symbol-value (intern name closure))))) closure)
    copy))

(defmacro make-closure ( &rest defines )
  "create a symbol table initializing SYMBOL with eval'd VALUE"
  (lexical-let
    ((table (make-vector (length defines) 0)))

    (mapc (lambda ( pair )
            (set (intern (symbol-name (car pair)) table) (eval (cadr pair)))) defines)
    table))

(provide 'mattie-elisp)
