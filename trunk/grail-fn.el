;;;----------------------------------------------------------------------
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

(defun quote-string-for-shell ( string )
  "quote-string-for-shell STRING

   quote the string with ' for the shell.
  "
  (concat "\'" string "\'"))

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

(defun is-current-frame-gui ( &optional frame-arg )
  "is-current-frame-x FRAME

   Return t if FRAME or (selected-frame) is a GUI frame, nil
   otherwise.
  "
  (let*
    ((frame (or frame-arg (selected-frame)))
     (frame-type (framep frame)))

;;    (message "frame type is %s" (princ frame-type))

    (when (and frame-type
            (or
              (equal 'x frame-type)
              (equal 'w32 frame-type)
              (equal 'ns frame-type)))
      t) ))

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
;; faces
;;----------------------------------------------------------------------

;; I previously used custom-theme-set-faces for setting faces, however
;; it broke with emacs --daemon, so I have created a macro to go
;; to use set-face-attribute.

(defun grail-set-face ( face attribute value )
  "grail-set-face FACE ATTRIBUTE VALUE

   set FACE attribute ATTRIBUTE to value. Attribute is a plain
   symbol 'foo' converted to a syntatic attribute ':foo' by this
   function.
  "
  (set-face-attribute face nil
    (read (concat ":" attribute)) value))

(defun pointer-to-face-p ( symbol )
  "pointer-to-face-p SYMBOL

   determine if SYMBOL is a variable that points to
   a face (t), or a face symbol (nil).
  "
  (condition-case nil
    (progn
      (eval symbol)
      t)
    (error
      nil)))

(defmacro grail-set-faces ( &rest list )
  "grail-set-faces BODY

   Set one or more faces with a list of attributes.
  "
  (let
    ((set-face-calls nil))

    (mapc
      ;; traverse the list of faces
      (lambda ( face )
        ;; traverse the list of attributes for a face.
        (mapc
          (lambda (attr-pair)
            ;; cons each attribute as a call to grail-set-face
            ;; onto a list of calls.
            (setq set-face-calls
              (cons
                `(grail-set-face
                   ,(if (pointer-to-face-p (car face))
                      (car face)
                      `',(car face))
                   ,(symbol-name (car attr-pair)) ,(cadr attr-pair))
                set-face-calls)))
          (cdr face)))
      list)
    (cons 'progn set-face-calls)))

;;----------------------------------------------------------------------
;; groups
;;----------------------------------------------------------------------

(defun grail-load-group ( group-name )
  (unless (load-elisp-if-exists (concat grail-local-groups group-name ".el"))
    (grail-dup-error-to-scratch
      (format "grail: group module %s aborted loading from errors" group-name)) ))

(defvar grail-requested-groups
  nil
  "List of group modules requested by the user.")

(defun grail-load-requested-groups ()
  "grail-load-requested-groups

   load the groups in the request list grail-requested-groups
   and then set the list to null, so that it can be re-run later.
  "
  (mapc 'grail-load-group
    grail-requested-groups)
  (setq grail-requested-groups nil))

(defun use-grail-groups ( &rest request-list )
  "use-grail-groups: LIST

   request a list of string quoted groups to be loaded after the configuration
   files have been loaded.
  "
  (setq grail-requested-groups
    (append request-list grail-requested-groups)))

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

(defmacro robust-load-elisp ( &rest load-expr )
  "robust-load-elisp LOAD-EXPR

   evaluate LOAD-EXPR trapping any errors that occur. the value
   of LOAD-EXPR is discarded, and t for successful, nil for
   errors is returned.
   "
  `(condition-case nil
     (progn
       ,@load-expr
       t)
     (error nil)) )

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

(defvar grail-save-downloads nil
  "when t downloaded archive files will be saved to grail-dist-dir")

(defun grail-recursive-delete-directory ( path )
  "grail-recursive-delete-directory PATH

   recursively delete the directory PATH. t on success, nil on error.
  "
  (condition-case trapped-error
    (if (dir-path-if-accessible path)
      (if (equal 0 (call-process-shell-command "rm" nil nil nil "-r" path))
        t
        nil)
      (progn
        (message "grail-recursive-delete-directory path %s is not a directory or the user does not have permissions" path)
        nil))
    (error
      (message "grail-recursive-delete-directory failed %s" (format-signal-trap trapped-error))
      nil)) )

(defun grail-tmp-dir-and-file-path ( name )
  (let
    ((tmp-dir  nil))

    (when (condition-case trapped-error
          (progn
            (setq tmp-dir (if grail-save-downloads
                            grail-dist-dir
                            (make-temp-file "grail" t)))
            t)
          (error
            (progn
              (message "grail could not create a temporary directory for temp path %s" name)
              nil)))
      (cons tmp-dir (grail-sanitize-path (concat tmp-dir "/" name))) )))

(defun grail-cleanup-download ( tmp-dir-and-file &optional ignore-save )
  "grail-cleanup-download

   delete the directory and the downloaded files.

   TODO: save downloads option.
  "
  (when tmp-dir-and-file
    (if grail-save-downloads
      ;; when grail-save-downloads is enabled absolutely do not recursive delete !
      (when (not ignore-save)
        (delete-file (cdr tmp-dir-and-file)))
      ;; otherwise it is a temp dir so nuke it
      (grail-recursive-delete-directory (car tmp-dir-and-file))) ))

(defun grail-process-async-chain ( start-process-fn doesnt-start-fn proc-fail-fn
                                   do-after-fn next-fn)
  "grail-process-async-chain START-PROCESS-FN DOESNT-START-FN PROC-FAIL-FN
                             DO-AFTER-FN NEXT-FN

   create asynchronous processes that can be changed. START-PROCESS-FN
   creates a process object. This function generates a process sentinel
   and attaches the sentinel to the process.

   a number of lambdas are supplied in the arguments to fill in the body
   of the process sentinel.

   DOESNT-START-FN: executed if the process does not start.

   PROC-FAIL-FN   : executed if the process returns an error (a non-zero exit code).
   DO-AFTER-FN    : executed when the process exits with success (0 exit code)
   NEXT-FN        : when DO-AFTER-FN returns non-nil this function is executed,
                    typically to chain another async process, but it can do
                    anything.

   With this function processes can be changed by nesting another
   grail-process-async-chain as the tail, or NEXT-FN function for
   a sequence of process execution.
  "
  (lexical-let
    ((async-proc (funcall start-process-fn))
     (no-start   doesnt-start-fn)
     (fail-fn    proc-fail-fn)
     (after-fn   do-after-fn)
     (chain-fn   next-fn))

    (if (or (not (processp async-proc))
            (not (process-status async-proc)))
      (funcall doesnt-start-fn)
      (progn
        ;; setup a lambda process sentinal that does the chaining.
        (set-process-sentinel async-proc
          ;; a sentinal that accepts status-change signals
          (lambda ( bound-proc status-change )
            (when (memq (process-status bound-proc) '(signal exit))
              ;; do something when the process exits
              (if (equal 0 (process-exit-status bound-proc))

                ;; If bound-proc process exits with success call the
                ;; do-after-exit function (do-after-fn).

                ;; If (do-after-fn) returns non-nil, and the (next-fn)
                ;; is non-nil run that function.
                (and (funcall after-fn) (and chain-fn (funcall chain-fn)))

                ;; if the process exits non-zero call (proc-fail-fn)
                (funcall fail-fn)) ))) ))))

(defun grail-wget-url-async ( url path output-buffer )
  "grail-wget-url-async URL PATH OUTPUT-BUFFER

   retrieve the URL to PATH, with OUTPUT-BUFFER as the output
   buffer. The process object created is returned, or nil if a
   process could not be created.
  "
  (condition-case trapped-error
    (start-process-shell-command "grail-wget" output-buffer
      "wget"
      "--progress=dot:binary"
      (quote-string-for-shell url) "-O" (quote-string-for-shell path))
    (error
      (progn
        (message "grail-wget-url failed %s" (format-signal-trap trapped-error))
        nil)) ))

(defun grail-untar-async ( path target-dir compression output-buffer )
  "grail-untar-async PATH DIR COMPRESSION OUTPUT-BUFFER

   untar PATH in DIR with output going to OUTPUT-BUFFER.
   return the process object or nil if there was an error.
  "
  (condition-case trapped-error
    (start-process-shell-command "grail-untar" output-buffer
      "tar"
      (concat
        "xv"
        (cond
          ((equal "gz"  compression) "z")
          ((equal "bz2" compression) "j")
          (signal error (format "grail: error! unsupported compression %s" compression)))
        "f")
      (quote-string-for-shell path)
      "-C" (quote-string-for-shell target-dir))
    (error
      (progn
        (message "grail-wget-url failed %s" (format-signal-trap trapped-error))
        nil)) ))

(defun grail-tarball-installer ( url name compression )
  "grail-tarball-installer

   Download a tarball and install it.
  "
  (save-excursion
    (lexical-let*
      ((tmp-dir-and-file nil)
       (old-window       (selected-window))

       ;; open a new window but do not put it in recently selected
       (grail-buffer  (pop-to-buffer (generate-new-buffer "*grail-install*") nil t))
       (grail-window  (not (eq old-window (selected-window)))))

      (catch 'abort
        ;; confirm with the user that they want to install the file.
        (unless (yes-or-no-p (format "download and install %s? " name))
          (throw 'abort nil))

        ;; signal the start of the download in the grail buffer.
        (insert (format "Starting the download of %s\n" url))

        ;; create a temporary directory to download into
        (unless (setq tmp-dir-and-file (grail-tmp-dir-and-file-path (concat name "." compression)))
          (throw 'abort "could not create a temporary directory for the download"))

        (lexical-let
          ((dl-url  url)
           (compression-type compression))

          (grail-process-async-chain
            ;; start the download with wget
            (lambda ()
              (grail-wget-url-async
                dl-url
                (cdr tmp-dir-and-file)
                grail-buffer))

            ;; the downloader doesn't start cleanup function
            (lambda ()
              (insert "could not start the download! Install aborted.\n")
              (grail-cleanup-download tmp-dir-and-file t))

            ;; the downloader fail cleanup function
            (lambda ()
              (grail-cleanup-download tmp-dir-and-file t)
              (message "download of %s failed! Install aborted, and downloads deleted." (cdr tmp-dir-and-file)))

            ;; the downloader succeeded function
            (lambda ()
              (insert "grail: download completed\n")
              t)

            ;; the chain function
            (lambda ()
              (grail-process-async-chain
                ;; start the untar
                (lambda ()
                  (message "starting the untar")
                  (grail-untar-async (cdr tmp-dir-and-file) grail-dist-elisp compression-type grail-buffer))

                ;; tar doesn't start cleanup function
                (lambda ()
                  (insert "could not start tar to extract the downloaded archive. Install aborted, deleting downloads.\n")
                  (grail-cleanup-download tmp-dir-and-file t))

                ;; the tar fail cleanup function
                (lambda ()
                  (insert (format "could not install files in %s from downloaded archive." grail-dist-elisp))
                  (grail-cleanup-download tmp-dir-and-file t))

                ;; the tar succeeded function
                (lambda ()
                  (insert "grail: Installation Completed ! Re-Generating load-path\n")
                  (grail-extend-load-path)

                  (insert "grail: cleaning up downloads\n")
                  (grail-cleanup-download tmp-dir-and-file)

                  (delete-windows-on grail-buffer)
                  (kill-buffer grail-buffer)
                  t)

                ;; terminate the chain.
                nil))) )
        ;; return nil if an abort is not thrown.
        nil))
    )) ;; save excursion and the defun.

(defun grail-install-file ( name url &optional path )
  "grail-install-file NAME URL PATH

   install from URL into PATH with name NAME.  nil is returned
   when successful, otherwise an error is thrown.
  "
  (condition-case error-trap
    (let
      ((install-path (grail-sanitize-path (concat
                                            (or path grail-dist-elisp)
                                            path "/" name ".el"))))

      (with-temp-buffer
        (url-insert-file-contents url)
        (write-file install-path))

      (message "grail-install-file: installed of %s to %s completed" name install-path))
    nil
    (error
      (format "grail-install-file for %s failed with: %s"
        name (format-signal-trap error-trap))) ))

(defun grail-define-installer ( def-symbol installer &optional install-dir )
  "grail-define-installer DEF-SYMBOL INSTALLER &optional INSTALL-DIR

   Define DEF-SYMBOL to INSTALLER, with the optional INSTALL-DIR property.

   INSTALL-DIR is a directory name when a sub-directory container for the
   installed files is desired.
  "
  (set def-symbol installer)
  (when install-dir
    (put def-symbol 'pkg-dir install-dir) ))

(defun grail-dist-install-directory ( &optional package )
  "grail-dist-install-directory &optional string:PACKAGE

   Ensure that the installation directory exists. The default is grail-dist-elisp,
   however for multi-file packages an optional package name can be supplied.

   The path of the installation directory is returned for the installer's use.
  "
  (grail-garuntee-dir-path (if package (concat grail-dist-elisp package "/") grail-dist-elisp)))


(defun grail-install-package ( name installer )
  "grail-dist-install-package NAME URL

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
            (grail-install-file (car ts-pair) (cdr ts-pair) install-to-dir) )
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
          ((or (listp installer) (symbolp installer)) (grail-install-package package installer)))

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

(defmacro grail-activate-with-recovery ( group package installer &rest init-code )
  "grail-load-dep-with-recovery string:GROUP symbol:PACKAGE INSTALLER code:INIT-CODE

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
                                      "grail: group module %s is degraded from %s loading failure %s"
                                      group
                                      pkg-name
                                      (format-signal-trap diagnostic)))
        (grail-repair-dependency-fn package (eval `'( ,@installer)))
        (throw 'abort t))

      ;; try the initialization trapping any errors.
      (when (setq diagnostic (diagnostic-load-elisp (funcall init-fn) ))
        (grail-dup-error-to-scratch (format
                                      "grail: group module %s is degraded from initialization error %s"
                                      group
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
    ((elpa-install (grail-install-file "package" (concat elpa-url "package.el"))))

    (when elpa-install
      (message "ELPA installation failed %s" elpa-install)))

  (unless (dir-path-if-accessible grail-dist-elpa)
    (make-directory grail-dist-elpa t)) )
