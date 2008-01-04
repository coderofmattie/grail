;;----------------------------------------------------------------------
;; mattie-boot.el
;; Primary Author: Mike Mattie
;;
;; definitions that are essential to the Emacs boot. This was split
;; from my general utility collection so that the risk of introducing
;; bugs/complexity early in the boot process could be minimized.
;;----------------------------------------------------------------------

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

(defun subdirs-of-path ( path path-type )
  "return a list of the subdirectories for a given path,
  excluding any directory that begins with a \".\".
  The two arguments are path and flag to choose relative (nil)
  or absolute (t) paths."

  ;; it is easier to read this from the bottom up.

  (apply 'map-filter-nil (lambda ( path-pair )
                           (if (and
                                 ;; only match directories
                                 (char-equal ?d (aref (cdr path-pair) 0))
                                 ;; exclude directories that start with "."
                                 (not (char-equal ?. (aref (car path-pair) 0))))
                             ;; when the criteria is met return the path only
                             (car path-pair)))

    ;; reduce the attributes to a pair of the path, and the mode string
    (mapcar (lambda ( attr-list )
              (cons (car attr-list) (nth 9 attr-list)))
      ;; get the list of files.
      (directory-files-and-attributes path path-type))))

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

(defun try-list (list)
  "iterate through the list of functions. If a function returns t for
   success terminate the iteration. It's a fancy or that assumes a list
   of functions."
  (catch 'terminate
    (dolist (func list)
      (if (funcall func)
        (throw 'terminate t)))
    nil
    ))
