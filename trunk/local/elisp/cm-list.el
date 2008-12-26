;;----------------------------------------------------------------------
;; cm-list.el
;; written by Mike Mattie
;;----------------------------------------------------------------------

(defun list-filter-nil ( list )
  "Filter nil symbols from a list"
  (remq 'nil list))

(defun seq-filter-nil ( &rest list-seq )
  "Filter nil symbols from a sequence."
  (list-filter-nil list-seq))

(defun precise-list-p ( x )
  "precise-list-p X

   more precise listp predicate. Checks that x is both a cons,
   and the cdr of x is a cons.
  "
  (and
    (consp x)
    (consp (cdr x))))

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

(defun or-fn-list ( list )
  "iterate through the list of functions. If a function returns t for
   success terminate the iteration. It's a fancy or that assumes a list
   of functions."
  (catch 'terminate
    (dolist (func list)
      (if (funcall func)
        (throw 'terminate t)))
    nil))

(defun join-as-list ( head tail )
  "join-as-list HEAD TAIL

   join HEAD and TAIL into a list. Both HEAD and TAIL can be a list or value.
   TAIL can be nil. HEAD must be non-nil.
  "
  (eval (list
          ;; The tail construction below ensures that the
          ;; tail is a list. complete-functions is a list
          ;; or a symbol, select the join function appropriately.
          (if (listp head)
            'append
            'cons)

          (quote head)

          ;; make a list either way out of the fallback.
          ;; either works for cons or append.
          (if (listp tail)
            (quote tail)
            (if tail
              (quote (cons tail nil))
              nil)) )))

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

(provide 'cm-list)
