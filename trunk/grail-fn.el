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

(defun grail-print-fn-to-scratch ( fn-name description )
  "grail-print-fn-to-scratch FN-NAME DESCRIPTION

   Print FN-NAME as a function call with DESCRIPTION instructions
   in the scratch buffer. The user can evaluate the description
   and easily un-comment the function and execute it.
  "
  (with-current-buffer "*scratch*"
    (goto-char (point-max))
    (insert (format "; (%s) ; un-comment and evaluate to %s\n" fn-name description))) )

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

;; find-library-name is not an auto-load so we need to force a load.
(require 'find-func)

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

(defun grail-install-url ( path name url )
  "grail-install-url PATH NAME URL

   install from URL into PATH with name NAME.  nil is returned
   when successful, otherwise an error is thrown.
  "
  (condition-case error-trap

    (with-temp-buffer
      (url-insert-file-contents url)
      (write-file (concat path name ".el")))

    (message "grail-install-url: would write to %s %s from %s" path name url)
    nil
    (error
      (format "grail-install-url for %s failed with: %s"
        name (format-signal-trap error-trap))) ))

(defun grail-install-package ( name installer )
  "grail-dist-install-file NAME URL

   download an elisp package named NAME from URL and install it in the user's dist directory.

   the install directory is returned on success or an error is thrown.
  "
  (let
    ((install-to-dir (grail-dist-install-directory (when (and (symbolp installer) (get installer 'pkg-dir))
                                                     (get installer 'pkg-dir))) )
     (install-data   (if (symbolp installer) (eval installer) installer)) )

    (cond
      ((listp install-data)
        (mapc
          (lambda ( ts-pair )
            ;; ts-pair is target source pair
            (grail-install-url install-to-dir (car ts-pair) (cdr ts-pair)) )
          install-data) )

      ((error "grail-install-package: un-handled type %s" (type-of install-data))) )
    install-to-dir))

;;----------------------------------------------------------------------
;; grail repair routines.
;;----------------------------------------------------------------------

(defun grail-repair-by-installing ( package installer )
  "grail-repair-by-installing symbol:PACKAGE string|function:INSTALLER

   Attempt to install PACKAGE and load the missing dependency. INSTALLER
   is either a URL string, or a custom installer function.

   t is returned on success and nil for failure.
  "
  (let
    ((package-name    (symbol-name package)))

    (catch 'installer-abort
      (condition-case install-trap

        (cond
          ((functionp installer) (funcall installer))
          ((listp   installer) (grail-install-package package installer)))

        (error
          (message "grail repair of package %s failed with %s" package-name (format-signal-trap install-trap))
          (throw 'installer-abort nil)) )

      (grail-extend-load-path)

      (condition-case load-trap
        (require package)
        (error
          (message "repair of package %s : installed ok, but loading failed anyways - %s."
            package-name (format-signal-trap load-trap))
          (throw 'installer-abort nil)) )

      (message "installation repair of dependency %s completed :)" package-name)
      t)))

(defun grail-repair-by-debugging ( package )
  "grail-repair-by-debugging symbol:PACKAGE

   Repair package loading by debugging.
  "
  (when (yes-or-no-p "repair: load the library source and enter the debugger on error ? ")
    (message
      "debug-on-error will be set. You may want to clear it after debugging with toggle-debug-on-error")

    (find-file-read-only-other-window (find-library-name (symbol-name package)))
    (setq debug-on-error t)
    (funcall 'require package) ))

(defun grail-repair-dependency-fn ( package installer )
  "grail-repair-dependency-fn PACKAGE INSTALLER

   Repair dependency loading problems by installation or by a
   entry point into a debugging work-flow.

   In essence this function selects between repairing by
   installation or debugging and generates the interactive
   function binding important variables such as the package.

   Based upon a search of load-path the error will be diagnosed
   either as a evaluation failure (if the library is found in
   load-path) or a missing installation.

   For a evaluation failure a debugging entry point is
   constructed. For a missing entry point an installer is
   generated.
  "
  (let*
    ((pkg-name (symbol-name package))
    ;; repair procedure is set to a diagnostic/function pair
    ;; format (string lambda) by cons.

     (repair-procedure
       (lexical-let
         ;; the package symbol needs to be lexically bound to the generated lambda's
         ((pkg-symbol package)
          (pkg-installer installer))

         (if (grail-in-load-path-p pkg-name)
           ;; if it is installed in load-path the library is aborting in
           ;; evaluation.

           (cons "%s is aborting during load on evaluation."
             (lambda ()
               (interactive)
               (grail-repair-by-debugging pkg-symbol)))

           (cons "%s cannot be found in the load-path"
             (lambda ()
               (interactive)
               (grail-repair-by-installing pkg-symbol pkg-installer))) )))

      (repair-fn-name (concat "repair-dependency-" pkg-name)) )

    ;; print the diagnostic to the scratch buffer
    (grail-dup-error-to-scratch (format (car repair-procedure) pkg-name))
    (grail-print-fn-to-scratch repair-fn-name (concat "install or initiate debugging of " pkg-name))

    (fset (intern repair-fn-name) (cdr repair-procedure)) ))

(defmacro grail-activate-with-recovery ( style package installer &rest init-code )
  "grail-load-dep-with-recovery string:STYLE symbol:PACKAGE INSTALLER code:INIT-CODE

   Attempt to load PACKAGE via require with error trapping, diagnosis, and repair.

   t is returned on success, nil on failure.
  "
  (let
    ((pkg-name     (symbol-name package))
     (diagnostic   nil)
     (load-fn `(lambda ()
                 (require ',package)))

     (init-fn `(lambda ()
                 (interactive)
                 ,@init-code)) )

    (if (catch 'abort ;; non-local exit for early termination from errors.

      ;; try loading the package catching any diagnostic errors from signals
      (when (setq diagnostic (diagnostic-load-elisp (funcall load-fn)))
        (grail-dup-error-to-scratch (format
                                      "grail: style %s is degraded from %s loading failure %s"
                                      style
                                      pkg-name
                                      (format-signal-trap diagnostic)))
        (grail-repair-dependency-fn package (eval `'( ,@installer)))
        (throw 'abort t))

      ;; try the initialization trapping any errors.
      (when (setq diagnostic (diagnostic-load-elisp (funcall init-fn) ))
        (grail-dup-error-to-scratch (format
                                      "grail: style %s is degraded from initialization error %s"
                                      style
                                      (format-signal-trap diagnostic)))
        (throw 'abort t))
        nil)

      (let
        ((init-fn-name (concat "initialize-" pkg-name)))

        (fset (intern init-fn-name) init-fn)

        (grail-print-fn-to-scratch init-fn-name (format "re-initialize %s after repair" pkg-name))
        nil)

      t) ))

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

