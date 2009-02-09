;;----------------------------------------------------------------------
;; grail-fn.el
;; Primary Author: Mike Mattie
;; Copyright (C) 2008 Mike Mattie
;; License: LPGL-v3
;;----------------------------------------------------------------------

;; definitions that are essential to the Emacs boot. This was split
;; from my general utility collection so that the risk of introducing
;; bugs/complexity early in the boot process could be minimized.

;;----------------------------------------------------------------------
;; basic utilities.
;;----------------------------------------------------------------------

(defun list-filter-nil ( list )
  "Filter nil symbols from a list"
  (remq 'nil list))

(defun seq-filter-nil ( &rest list-seq )
  "Filter nil symbols from a sequence."
  (list-filter-nil list-seq))

(defun map-filter-nil ( func &rest seq )
  "map-filter-nil FUNC LIST

   Filter the nil elements of LIST from the input and output of
   function FUNC.

   FUNC is applied to the non-nil elements of SEQ ala mapcar. The
   result is either a list or nil if filtering eliminated all
   output."
  (lexical-let
    ((rvalue nil))

    (dolist (element seq)
      (when element
        (lexical-let
          ((transform (funcall func element)))
          (when transform
            (push transform rvalue)))))
    (reverse rvalue)))

;;----------------------------------------------------------------------
;; styles
;;----------------------------------------------------------------------

(defun load-style ( style-name )
  (unless (load-elisp-if-exists (concat grail-local-styles style-name ".el"))
    (grail-dup-error-to-scratch
      (format "grail: style %s aborted loading from errors" style-name)) ))

(defvar requested-styles-list
  nil
  "List of styles requested by the user.")

(defun load-requested-styles ()
  (mapc 'load-style
    requested-styles-list))

(defun use-styles ( &rest request-list )
  (mapc
    (lambda ( name )
      (push name requested-styles-list))
    request-list))

;;----------------------------------------------------------------------
;; filter-ls: a general purpose tools for filtering directory listings.
;;----------------------------------------------------------------------

(defun filter-ls-predicate ( attr-name attr-match )
  "create predicate filters for path/mode values"
  (cond
    ((string-equal "type" attr-name) `(char-equal ,attr-match  (aref (cdr path-pair) 0)))
    ((string-equal "path" attr-name) `(string-match ,attr-match (car path-pair)))
    ((string-equal "name" attr-name) `(string-match ,attr-match (file-name-nondirectory (car path-pair)))) ))

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
  "filter-ls PATH PATH-TYPE
  a form for flexibly filtering the result of listing a directory with attributes

   t   absolute paths
   nil relative paths"
  `(apply 'map-filter-nil
     (lambda ( path-pair )
       (if ,(cons 'and (mapcar 'filter-ls-attributes filters))
         (car path-pair)))

     ;; reduce the attributes to a pair of the path, and the mode string
     (mapcar (lambda ( attr-list )
               (cons (car attr-list) (nth 9 attr-list)))
       ;; get the list of files.
       (directory-files-and-attributes ,path ,path-type)) ))

;;----------------------------------------------------------------------
;; diagnostic support routines.
;;----------------------------------------------------------------------

(defmacro diagnostic-load-elisp ( &rest load-expr )
  "robust-load-elisp LOAD-EXPR

   evaluate LOAD-EXPR trapping any errors that occur. the value
   of LOAD-EXPR is discarded, and nil for a succesful load, or
   the trapped error is returned.
   "
  `(condition-case error-trap
     (progn
       ,@load-expr
       nil)
     (error error-trap)) )

(defun format-signal-trap (signal-trap)
  "format-signal-trap list:SIGNAL-TRAP

   format SIGNAL-TRAP for use in error messages.
  "
  (format "(%s , \"%s\")" (symbol-name (car signal-trap)) (cadr signal-trap)) )

(defun grail-in-load-path-p (package)
  "grail-in-load-path-p elisp-name

   Return either the absolute path to the elisp-file if it is found
   in load-path, or nil otherwise.
  "
  (condition-case nil
    (find-library-name package)
    (error nil)) )

;;----------------------------------------------------------------------
;; installation routines.
;;----------------------------------------------------------------------

(defun grail-dist-install-directory ( &optional package )
  "grail-dist-install-directory &optional string:PACKAGE

   Ensure that the installation directory exists. The default is grail-dist-elisp,
   however for multi-file packages an optional package name can be supplied.

   The path of the installation directory is returned for the installer's use.
  "
  (let
    ((install-directory (if package (concat grail-dist-elisp package) grail-dist-elisp)))

    (unless (dir-path-if-accessible install-directory)
      (make-directory install-directory t))
    install-directory))

(defun grail-dist-install-file ( name url )
  "grail-dist-install-file NAME URL

   download an elisp file named NAME.el from URL and install it in the user's dist directory.

   The error message is returned if there is an error, nil for sucess.
  "
  (grail-dist-install-directory)

  (condition-case error-trap
    (with-temp-buffer
      (url-insert-file-contents url)
      (write-file (concat grail-dist-elisp name ".el")))

    nil
    (error
      (format "grail-dist-install-file for %s failed with: %s"
        name (format-signal-trap error-trap))) ))

;;----------------------------------------------------------------------
;; ELPA
;;
;; The preferred way to install software is with ELPA which is a
;; sophisticated package management system.
;;
;; the grail install functions are overly simplistic in comparison.
;;----------------------------------------------------------------------

(defconst elpa-url
  "http://tromey.com/elpa/")

(defun load-elpa-when-installed ()
  "load-elpa-when-installed

   If the ELPA package management system http://tromey.com/elpa/ is installed,
   configure it for use, assuming a proper install by grail-install-elpa.
  "
  (interactive)
  (when (and
         (file-accessible-directory-p grail-dist-elpa)
         (load-elisp-if-exists (concat grail-dist-elisp "package.el")))

    ;; FIXME: I need to defadvise package-do-activate to capture the load-path changes made
    ;; by elpa

    (setq-default package-user-dir grail-dist-elpa)
    (push grail-dist-elpa package-directory-list)
    (package-initialize)))

(defun grail-install-elpa ()
  "install the ELPA package management system"
  (interactive)

  (let
    ((elpa-install (grail-dist-install-file "package" (concat elpa-url "package.el"))))

    (when elpa-install
      (message "ELPA installation failed %s" elpa-install)))

  (unless (dir-path-if-accessible grail-dist-elpa)
    (make-directory grail-dist-elpa t)) )

