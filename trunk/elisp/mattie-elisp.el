;;----------------------------------------------------------------------
;; mattie-elisp.el
;; Primary Author: Mike Mattie
;; Copyright (C) 2008 Mike Mattie
;; Description: basic elisp programming tools.
;;----------------------------------------------------------------------

(defun join-cons ( a b )
  "like cons but joins as a list instead of nesting"
  (let
    ((new-a (if (cdr a) (cons a nil) a))
     (new-b (if (cdr b) (cons b nil) b)) )
    (setcdr new-a new-b)
    new-a))

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
     ,docstring)
  )

(defun mode-overlay-at-point-p ( mode-symbol )
  "determine if the point is in a flyspell overlay. given a overlay list
   which may be nil, translate via predicate into boolean values which
   are then evaluated by or."
  (interactive)
  (let
    ((overlay-list (overlays-at (point))))

    (if overlay-list
      (eval (cons 'or
              (mapcar
                (lambda ( overlay )
                  (if (overlay-get overlay mode-symbol) t)) overlay-list)
              ))
    )))

;; I really like this implementation, would map-filter-nil benefit from
;; using consp ?
(defun list-filter-nil ( list )
  "filter nil symbols from a list"
  (if (consp list)
    (lexical-let
      ((head (car list)))

      (if (eq head 'nil)
        (list-filter-nil (cdr list))
        (cons head (list-filter-nil (cdr list)))
        ))
    nil
    ))

;; define-error originated in XEmacs. This implementation shares the
;; same name, but not the interface. I need to clone or copy the
;; XEmacs version.

(defmacro define-error ( symbol message &rest isa-list )
  "define a error symbol with a isa list and a error message"
  `(progn
     (put ',symbol
       'error-conditions (append '(error ,symbol) ',isa-list))
     (put ',symbol 'error-message ,message)
     ))

(provide 'mattie-elisp)
