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

(defun eqn ( a b )
  "eqn A B

   A and B are equal if their symbol names are a string match.
  "
  (string-equal (symbol-name a) (symbol-name b)))

(defun precise-list-p ( x )
  "precise-list-p X

   more precise listp predicate. Checks that x is both a cons,
   and the cdr of x is a cons.
  "
  (and
    (consp x)
    (consp (cdr x))))

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

;; could use remq 'nil list
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

(defun consume-list ( list consume )
  "consume-list LIST CONSUME

   use a consume function to consume a list. unlike mapc instead
   of consuming only a single element of the list at a time the
   head, and the tail are passed to CONSUME so that a single call
   can consume a variable n elements from list.

   This function amounts to a TCO kludge allowing the function to
   be expressed in a recursive form."
  (when list
    (while (setq list (funcall consume (car list) (cdr list)))))
  nil)

(defun apply-n-times ( func n x )
  "apply-n-times FUNC N X

   apply FUNC to X N times, With X set to the return
   value of FUNC for each iteration.
  "
  (while (> n 0)
    (setq x (funcall func x))
    (decf n))
  x)

(defun split-list ( n list )
  "split-list N LIST
   return a cons of LIST split into two lists at index N.
  "
  (if (> n 1)
    (lexical-let
      ((a-list list)
        (b-list nil)
        (before-split (apply-n-times 'cdr (- n 1) list)))

      (setq b-list (cdr before-split))
      (setcdr before-split nil)

      (cons a-list b-list))
    (cons (cons (car list) nil) (cdr list))))

;;----------------------------------------------------------------------
;; tail iterator
;;----------------------------------------------------------------------

(defun tail-iterator-merge ( a b )
  (setcdr a b)

  (do ((x b))
    ((null (cdr x)) x)
    (setq x (cdr x))))

(defun tail-iterator ( bind-to )
  (set bind-to (cons nil nil))

  (lexical-let
    ((tail (symbol-value bind-to)))

    (lambda ( x )
      (if (precise-list-p x)
        (setq tail (tail-iterator-merge tail x))
        (progn
          (setcdr tail (cons x nil))
          (setq tail (cdr tail))) )) ))

(defun tail-list ( list )
  (cdr list))


(provide 'mattie-elisp)
