;;----------------------------------------------------------------------
;; load-library.el
;; Primary Author: Mike Mattie
;;
;; definitions that are essential to the Emacs boot. This was split
;; from my general utility collection so that the risk of introducing
;; bugs/complexity early in the boot process could be minimized.
;;----------------------------------------------------------------------

(require 'cl) ;; need the common-lisp macros such as lexical-let

;;----------------------------------------------------------------------
;; String handling functions oriented towards manipulating path lists.
;; These are essential for the earliest part of the init process,
;; modifying the library loading path.
;;----------------------------------------------------------------------

(defun string-join (prefix list)
  ;; This is analgous to the perl5 join function.
  ;; given a <prefix> and a <list> of strings join the
  ;; strings with <prefix> as a seperator between the
  ;; list values.
  ;;
  ;; The result is a single string value.
  (concat
    prefix (car list)
    (if (cdr list) (string-join prefix (cdr list)))
    ))

(defun string-prefix-list (prefix list)
  (cons
    (concat prefix (car list))

    (cond
      ((cdr list) (string-prefix-list prefix (cdr list)))
       ('()))
      ))

(defun path-join (list)
  (concat
    (car list) ":"
    (if (cdr list) (string-join (cdr list)))
    ))

;;----------------------------------------------------------------------
;; clean - these implementations are considered clean in that they
;;         are documented and not ugly.
;;----------------------------------------------------------------------

(defun map-filter-nil ( func &rest seq )
  "map-filter-nil. apply the function to the arguements ala mapcar.
   Filter any nil elements of the sequence before the function is
   applied, and after the function is applied."

  (if (car seq)
    (let
      ((result (funcall func (car seq))))
      (if result
        (cons result (apply 'map-filter-nil func (cdr seq)))
        (apply 'map-filter-nil func (cdr seq))
        ))
    (if (cdr seq)
      (apply 'map-filter-nil func (cdr seq)))
    ))

;;----------------------------------------------------------------------
;; filter-ls: a general purpose tools for filtering directory listings.
;;----------------------------------------------------------------------

(defun filter-ls-predicate ( attr-name attr-match )
  "create predicate filters for path/mode values"
  (cond
    ((string-equal "type" attr-name) `(char-equal ,attr-match  (aref (cdr path-pair) 0)))
    ((string-equal "path" attr-name) `(string-match-p ,attr-match (car path-pair)))
  ))

(defun filter-ls-attributes ( filter-form )
  "implement the various attribute filters for the filter-ls form"
  (lexical-let
    ((attr-name (symbol-name (car filter-form)))
      (attr-match (cadr filter-form)))

    (if (char-equal ?! (aref attr-name 0))
      (list 'not (filter-ls-predicate (substring attr-name 1) attr-match))
      (filter-ls-predicate attr-name attr-match))
    ))

(defmacro filter-ls (path path-type &rest filters)
  "a form for flexibly filtering the result of listing a directory with attributes"
  `(apply 'map-filter-nil
     (lambda ( path-pair )
       (if ,(cons 'and (mapcar 'filter-ls-attributes filters))
         (car path-pair)))

     ;; reduce the attributes to a pair of the path, and the mode string
     (mapcar (lambda ( attr-list )
               (cons (car attr-list) (nth 9 attr-list)))
       ;; get the list of files.
       (directory-files-and-attributes ,path ,path-type))
     ))

(defun simple-set-expand-prop ( set-op spec )
  "beta"
  (if (string-equal (symbol-name set-op) "splice")
    ;; when multiple properties are specified splice them in.
    spec

    ;; when a single property is specified create the property
    ;; syntax. The property name is constructed with read. The
    ;; specification is eval'd to render it essentially
    ;; typeless. Symbols now require quoting.
    (list (read (concat ":" (symbol-name set-op))) (eval spec))))

(defmacro simple-set-theme ( theme &rest given-forms )
  "create the syntax for custom-theme-set-faces"
  (append
    `(custom-theme-set-faces ',theme)

     (mapcar
       (lambda (form)
         `'(,(car form)
             ((t
                ,(simple-set-expand-prop (cadr form) (caddr form))
                )))
         ) given-forms)
    ))
