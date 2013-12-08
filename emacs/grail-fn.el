;;;----------------------------------------------------------------------
;; grail-fn.el
;;----------------------------------------------------------------------

;; grail-fn is a library of functions required by grail to boot. These
;; functions are seperated from grail itself to minimize the
;; opportunities for errors to occur in the earliest stage of loading,
;; and to facilitate compilation.

(eval-when-compile
  (require 'cl))

;;----------------------------------------------------------------------
;; general lisp functions
;;----------------------------------------------------------------------

;;
;; lists
;;

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
  (let
    ((rvalue nil))

    (dolist (element seq)
      (when element
        (let
          ((transform (funcall func element)))
          (when transform
            (push transform rvalue)))))
    (reverse rvalue)))

;;
;; error handling
;;

(defun format-signal-trap (signal-trap)
  "format-signal-trap list:SIGNAL-TRAP

   format SIGNAL-TRAP for use in error messages.
  "
  (format "(%s , \"%s\")"
    (symbol-name (car signal-trap))

    (if (listp (cdr signal-trap))
      (cadr signal-trap)
      (cdr signal-trap)) ))


(defun grail-insert-error ( message )
  (let
    ((message-start (point)))

    (cond
      ((stringp message) (insert message))
      ((functionp message) (insert "(%s)" (princ message)))
      (t (insert "%s" (princ message))) )

    (fill-region message-start (point))

    (insert "\n\n")))

(defun grail-report-errors ( message &rest errors )
  "grail-report-errors ERROR-MESSAGE

  duplicate the ERROR-MESSAGE to both *Messages* as a log and to the
  *scratch* buffer as a comment where it is highly visible.
  "

  (with-current-buffer "*scratch*"
    (let
      ((error-start (progn
                      (goto-char (point-max))
                      (insert "\n")
                      (point)) ))

      (grail-insert-error message)

      (when (not (null errors))
        (let
          ((cause-start (point)))

          (mapc
            (lambda (cause)
              (grail-insert-error (format "*  %s" cause)))
            errors)

          (insert "\n")
          (let
            ((fill-prefix "  "))
            (indent-region cause-start (point))) ))

      (let
        ((comment-start ";"))

        (comment-region error-start (point))) )) )

;;
;; path functions
;;

(defun dir-path-if-accessible ( path )
  "return the path if the directory is readable, otherwise nil"
  (if (and path (file-accessible-directory-p path))
    path
    nil))

(defun file-path-if-readable ( file )
  "return the path if the file is readable, otherwise nil"
  (if (file-readable-p file)
    file))

(defun grail-garuntee-dir-path ( path )
  "grail-garuntee-dir-path PATH

   If the directory PATH does not already exist then create it.
   return the path of the directory or nil.
  "
  (or (dir-path-if-accessible path)
    (progn
      (make-directory path t)
      path)) )

;;----------------------------------------------------------------------
;; interface detection
;;----------------------------------------------------------------------

(defun is-current-frame-gui ( &optional frame-arg )
  "is-current-frame-x FRAME

   Return t if FRAME or (selected-frame) is a GUI frame, nil
   otherwise.
  "
  (let*
    ((frame (or frame-arg (selected-frame)))
     (frame-type (framep frame)))

    (when (and frame-type
            (or
              (equal 'x frame-type)
              (equal 'w32 frame-type)
              (equal 'ns frame-type)))
      t) ))

;;----------------------------------------------------------------------
;; loading functions
;;----------------------------------------------------------------------

(defun load-elisp-trapping-errors ( path )
  "load-elisp-file-trapping-errors PATH

   load PATH throwing grail-trap with diagnostics if an error is
   reported by diagnostic-load-elisp-file.

   t is returned on success, nil on failure.
  "
  (let
    ((diagnostics (diagnostic-load-elisp-file path)))

    (if diagnostics
      (throw 'grail-trap (list
                           (format "grail: %s aborted loading" path)
                           (format-signal-trap diagnostics)))
      t) ))

(defun load-elisp-if-exists ( path )
  "load-elisp-if-exists PATH

   Try to load the elisp file PATH only if it exists and is
   readable.

   t is returned if the file was found and loaded without
   errors, nil otherwise.
  "
  (let
    ((checked-path (or (file-path-if-readable path)
                       (file-path-if-readable (concat path ".elc"))
                       (file-path-if-readable (concat path ".el")) )))

    (if checked-path
      (let
        ((trap (catch 'grail-trap (load-elisp-trapping-errors checked-path))))

        (when (consp trap)
          (throw 'grail-trap (cons "grail: unexepected error loading an existing path; likely a syntax problem, or a missing require"
                               trap)))
        t)
      nil)))

(defun load-user-elisp ( file )
  "load-user-elisp FILE

   A fully guarded load that checks for a non-nil FILE name
   and attempts to load it relative to grail-elisp-root.

   t is returned if the file was found and loaded without
   errors, nil otherwise.
  "
  (when file
    (load-elisp-if-exists (concat grail-elisp-root file))))

;;
;; display/gui loading
;;

(defvar grail-display-configured nil
  "Boolean for if grail has configured the frame.")

(defvar grail-gui-configured nil
  "Boolean for if grail has configured the gui.")

(defun grail-load-gui-configuration-once ( frame )
  "grail-load-gui-configuration-once

   Load the GUI configuration file gui.el setting a flag to
   ensure that multiple calls only load the file once so that
   this function can be safely placed on a hook.

   It ignores an optional parameter so that it can be placed on
   after-make-frame-functions.
  "
  (when (and (not grail-gui-configured) (is-current-frame-gui frame))
    (load-user-elisp "gui")
    (setq grail-gui-configured t)) )

(defun grail-load-display-configuration-once ()
  "grail-load-display-configuration-once

   Load the display configuration file display.el only once,
   before a frame is created ala grail-load-gui-configuration-once.
  "
  (unless grail-display-configured
    (load-user-elisp "display")
    (setq grail-display-configured t)))

;;----------------------------------------------------------------------
;; load-path construction
;;----------------------------------------------------------------------

(defun filter-path-by-attributes ( path filter-name filter-value )
  "create predicate filters for path/mode values"

  (catch 'result
    (cond
      ((string-equal "type" filter-name)
        (let
          ((file-type (nth 1 path)))

          (cond
            ((and (eq file-type t) (string-equal filter-value "dir")) (throw 'result t))
            ((and (stringp file-type) (string-equal filter-value "lnk")) (throw 'result t))
            ((and (eq file-type nil) (string-equal filter-value "file")) (throw 'result t)) ) ))

      ((string-equal "path" filter-name)
        (when (equal 0 (string-match filter-value (car path)))
          (throw 'result t))) )
    nil))

(defmacro filter-path-with-match-sense ( path filter &rest body )
  `(if (eq 'not (car ,filter))
     (when (filter-path-by-attributes ,path (car (cdr ,filter)) (car (cdr (cdr ,filter))))
       ,@body)
     (unless (filter-path-by-attributes ,path (car ,filter) (car (cdr ,filter)))
       ,@body) ) )

(defun filter-dir-by-attributes ( directory filters )
  (let
    ((contents (directory-files-and-attributes directory ))
     (filtered nil))

    (mapc (lambda ( path )
            (catch 'eliminate
              (mapc (lambda ( filter )
                      (filter-path-with-match-sense path filter
                        (throw 'eliminate t)))
                filters)

              (setq filtered (cons path filtered)) ))
      contents)

    (mapcar (lambda ( path-with-attributes )
              (concat directory "/" (car path-with-attributes)) )
      filtered) ))

(defun grail-filter-directory-list ( dir-list )
  (let
    ((exists-list nil))

    (if (listp dir-list)
      (mapc (lambda ( dir )
              (when (dir-path-if-accessible dir)
                (setq exists-list (cons dir exists-list))) )
        dir-list)
      (when (dir-path-if-accessible dir-list)
	(setq exists-list (list dir-list))) )
    exists-list))

(defun grail-extend-load-path ()
  "grail-extend-load-path

   build extended-load-path in override order highest -> lowest with:

   --- override ---

   1. grail-local-emacs   - local, for preferring local modifications of mainline packages.
   2. emacs-load-path     - the emacs boot load path

   --- extend ---

   3. grail-local-elisp   - user written elisp
   4. elpa-load-path      - elpa managed third party packages
   5. grail-dist-elisp    - grail managed third party packages

   non-existent directories are filtered out.
  "
  (let
     ((new-load-path nil)

      (gather-load-path
        (lambda ( dir &optional subdirs )
          (let
            ((filtered-load-path nil))

            (setq filtered-load-path
              (if subdirs
                (when (file-accessible-directory-p dir)
                  (filter-dir-by-attributes dir
                    '( ("type" "dir")
                     (not "path" "^\.\.?$")) ))
                (grail-filter-directory-list dir)) )

            (when filtered-load-path
              (setq new-load-path (append filtered-load-path new-load-path))) )) ))

    (funcall gather-load-path grail-platform-load-path)

    (funcall gather-load-path grail-local-emacs)

    (funcall gather-load-path grail-local-elisp)

    (funcall gather-load-path grail-dist-elisp)

    (funcall gather-load-path grail-elpa-load-path)

    (funcall gather-load-path grail-dist-cvs 't)

    (funcall gather-load-path grail-dist-git 't)

    (funcall gather-load-path grail-dist-bzr 't)

    (if new-load-path
      (setq load-path new-load-path)) ))

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
;; ELPA
;;
;; The preferred way to install software is with ELPA which is a
;; sophisticated package management system.
;;
;; the grail install functions are overly simplistic in comparison.
;;----------------------------------------------------------------------

(defconst elpa-url "http://repo.or.cz/w/emacs.git/blob_plain/1a0a666f941c99882093d7bd08ced15033bc3f0c:/lisp/emacs-lisp/")

;(defconst elpa-url "http://tromey.com/elpa/")

(defun load-elpa-when-installed ()
  "load-elpa-when-installed

   If the ELPA package management system http://tromey.com/elpa/ is installed,
   configure it for use, assuming a proper install by grail-install-elpa.

   t is returned if succesful, otherwise nil is returned.
  "
  (interactive)
  (if (or (when (>= emacs-major-version 24) (require 'package nil t))
          (load-elisp-if-exists (concat grail-dist-elisp "package")))
    (progn
      (unless (dir-path-if-accessible grail-dist-elpa)
        (make-directory grail-dist-elpa t))

      (setq-default package-user-dir grail-dist-elpa)
      (push grail-dist-elpa package-directory-list)

      ;; ELPA is loaded so do the ugly parts and hook into package.el's guts
      ;; to pick up it's modifications to load-path

      (defadvice package-activate-1 (after grail-snoop/do-activate)
        (let
          ((snooped (car load-path))) ;; elpa always cons's the new path on the front.
          (when snooped
            (message "grail: snooped load-path update %s from package.el" snooped)
            (setq grail-elpa-load-path (cons snooped grail-elpa-load-path))
            (grail-extend-load-path))
          ))

      (ad-activate 'package-activate-1)

      (let
        ((elpa-errors (diagnostic-load-elisp (package-initialize))))

        (if elpa-errors
          (grail-report-errors "ELPA failed to initialize" elpa-errors)
          t)) )
    nil))

(defun grail-install-elpa ()
  "install the ELPA package management system"
  (interactive)

  (catch 'abort
    (unless grail-local-profiles
      (message "installing ELPA requires loading grail-profile.el for installation routines. Please consult README.grail and place grail-profile.el in USER_ELISP")
      (throw 'abort))

    (let
      ((elpa-install (grail-run-installer
                       (grail-define-installer "package"
                         "file"
                         (concat elpa-url "package.el"))) ))

      (unless elpa-install
        (message "ELPA installation failed %s" elpa-install)))

    (load-elpa-when-installed) ))

(provide 'grail-fn)
