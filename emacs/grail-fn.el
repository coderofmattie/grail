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
  "Boolean for if grail has configured the display.")

(defvar grail-font-size 24)

(defun grail-load-display-configuration-once ( &rest ignore )
  "grail-load-display-configuration-once

   Load the display configuration file display.el only once,
   before a frame is created ala grail-load-gui-configuration-once.
  "
  (unless grail-display-configured
    (load-user-elisp "user-display")
    (setq grail-display-configured t)) )

(defvar grail-graphical-configured nil
  "Boolean for if grail has configured the frame.")

(defun grail-select-best-font-family ( &optional frame )
  (catch 'best-font
    (dolist (canidate platform-font-family)
      (when (member canidate (font-family-list frame))
        (throw 'best-font canidate)) )

    (car (font-family-list frame)) ))

(defun grail-format-font ( family size &optional frame )
  (let
    (( font-spec (aref (font-info family frame) 1) ))
    (concat (replace-regexp-in-string "pixelsize=[[:digit:]]+" (format "pixelsize=%s" size) font-spec) ":spacing=m") ))

(defun grail-setup-frame-font ( font-spec &optional frame )
  (set-face-attribute 'default frame
    :font font-spec) )

(defun grail-setup-frame-attributes ( &optional frame )
  (set-face-attribute 'default frame :underline nil)
  (set-face-attribute 'default frame :inverse-video nil)
  (set-face-attribute 'default frame :box nil)
  (set-face-attribute 'default frame :strike-through nil)
  (set-face-attribute 'default frame :overline nil)

  (set-face-background 'cursor "yellow" frame) )

(defun grail-load-graphical-configuration-once ( &optional frame )
  (when (and (not grail-graphical-configured) (is-current-frame-gui frame))

     (let
       (( font-spec (grail-format-font (grail-select-best-font-family frame) grail-font-size frame) ))

       ;; setup the current frame with attributes
       (grail-setup-frame-font font-spec frame)
       (grail-setup-frame-attributes frame)

       ;; setup subsequent frames with the frame alist
       (setq
         default-frame-alist
         `((font . ,font-spec)
           (underline . nil)
           (inverse-video . nil)
           (box . nil)
           (strike-through . nil)
           (overline . nil)
           (cursor-color . "yellow") )

         ;; misc graphical settings
         use-dialog-box nil
         x-select-enable-clipboard t) )

    (setq-default cursor-type "box")

    (setq grail-graphical-configured t) ))

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

(defun grail-recurse-load-path ( dir )
  (if (file-accessible-directory-p dir)
    (let
      (( elisp-files
         (filter-dir-by-attributes dir
           '(("type" "file")
             ("path" ".*\.elc?$"))) )
        ( elisp-dirs nil ))

      (when elisp-files
        (setq elisp-dirs (cons dir elisp-dirs)))

      (let
        (( sub-dirs
           (filter-dir-by-attributes dir
             '(("type" "dir")
               (not "path" "^\.\.?$"))) ))

        (when sub-dirs
          (mapc
            (lambda ( dir )
              (let
                (( next-level (grail-recurse-load-path dir) ))

                (when next-level
                  (setq elisp-dirs (append next-level elisp-dirs))) ))
            sub-dirs)) )
      elisp-dirs)
    nil))

(defmacro grail-new-load-path ( &rest body )
  `(let
     (( new-load-path nil )
      ( search-results nil ))

     ,@(mapcar
        (lambda ( path )
          `(progn
             (setq search-results
               (if (listp ,path)
                 (grail-filter-directory-list ,path)
                 (grail-recurse-load-path ,path)))

             (when search-results
               (setq new-load-path (append search-results new-load-path))) ))
         body)

     new-load-path))

(defvar grail-dist-path-table (make-hash-table :test 'equal))

(defun grail-update-dist-path-table ( dir-list )
  (mapc
    (lambda ( dir )
      (when (file-accessible-directory-p dir)
        (let
          (( sub-dirs
             (filter-dir-by-attributes dir
               '(("type" "dir")
                 (not "path" "^\.\.?$"))) ))

          (when sub-dirs
            (mapc
              (lambda ( sub-dir )
                (let
                  (( dir-name (file-name-nondirectory sub-dir) ))
                  (message "dir-name is \"%s\"" dir-name)

                  (unless (gethash dir-name grail-dist-path-table)
                    (message "putting in the hash %s"sub-dir)
                    (puthash dir-name sub-dir grail-dist-path-table)) ))
              sub-dirs)) )))
      dir-list))

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
    ((new-load-path
       (grail-new-load-path
         ;;----------------------------------------------------------------------
         ;; user elisp code for platform, emacs, and user elisp
         ;;----------------------------------------------------------------------
         grail-platform-load-path

         grail-local-elisp

         ;;----------------------------------------------------------------------
         ;; 3rd party elisp
         ;;----------------------------------------------------------------------

         grail-dist-elisp
         grail-elpa-load-path

         ;;----------------------------------------------------------------------
         ;; upstream source. repos can be deep requiring a search of
         ;; subdirectories
         ;;----------------------------------------------------------------------

         grail-dist-cvs
         grail-dist-git
         grail-dist-bzr )))

    (when new-load-path
      (setq load-path (reverse new-load-path)))

    (grail-update-dist-path-table (list grail-dist-cvs grail-dist-git grail-dist-bzr)) ))

(defun grail-find-package-resource ( package resource )
  (catch 'found
    (let
      (( package-dir (or (dir-path-if-accessible package)
                         (gethash package grail-dist-path-table) )))

      (if (file-accessible-directory-p package-dir)
        (let
          (( found-paths
             (filter-dir-by-attributes package-dir
               `(("path" ,resource))) ))

          (when found-paths
            (throw 'found found-paths))

          (let
            (( sub-dirs
               (filter-dir-by-attributes package-dir
                 '(("type" "dir")
                    (not "path" "^\.\.?$"))) ))

            (when sub-dirs
              (mapc
                (lambda ( dir )
                  (let
                    (( next-level (grail-find-package-resource dir resource) ))

                    (when next-level
                      (throw 'found next-level)) ))
                sub-dirs)) ))
        nil) )))

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
