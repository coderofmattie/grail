;;;----------------------------------------------------------------------
;; grail-load.el
;;----------------------------------------------------------------------

;; grail-fn is a library of functions required by grail to boot. These
;; functions are seperated from grail itself to minimize the
;; opportunities for errors to occur in the earliest stage of loading,
;; and to facilitate compilation.
(require 'cl)

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
;; load-path construction
;;

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
              (when (grail-dir-if-ok dir)
                (setq exists-list (cons dir exists-list))) )
        dir-list)
      (when (grail-dir-if-ok dir-list)
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
                  ;; (message "dir-name is \"%s\"" dir-name)

                  (unless (gethash dir-name grail-dist-path-table)
                    ;; (message "putting in the hash %s" sub-dir)
                    (puthash dir-name sub-dir grail-dist-path-table)) ))
              sub-dirs)) )))
      dir-list))

(defun grail-update-load-path ()
  "grail-update-load-path

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
         grail-dist-bzr
         grail-dist-hg )))

    (when new-load-path
      (setq load-path (reverse new-load-path)))

    (grail-update-dist-path-table (list grail-dist-cvs grail-dist-git grail-dist-bzr grail-dist-hg)) ))

(defun grail-find-package-resource ( package resource )
  (catch 'found
    (let
      (( package-dir (or (grail-dir-if-ok package)
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

;;
;; user interface loading
;;

(defun is-frame-gui ( &optional this-frame )
  "is-current-frame-x FRAME

   Return t if FRAME or (selected-frame) is a GUI frame, nil
   otherwise.
  "
  (let
    ((frame-type (framep this-frame)))

    (when (and frame-type
            (or
              (equal 'x frame-type)
              (equal 'w32 frame-type)
              (equal 'ns frame-type)))
      t) ))

;;
;; frame configuration before first frame
;;

(defvar grail-display-configured nil
  "Boolean for if grail has configured the display.")

(defun grail-configure-display ()
  "grail-configure-display

   Load the display configuration before the initial frame.
   This is done with a sentry so it only runs once.
  "
  (unless grail-display-configured
    (grail-try-user-elisp "configure-display")
    (setq grail-display-configured t) ))

(defun grail-merge-frame-parameters ( emacs-alist-sym &rest pairs )
  (let
    (( emacs-alist (eval emacs-alist-sym) ))

    (mapc
      (lambda ( x )
        (setq emacs-alist (assq-delete-all (car x) emacs-alist)) )
      pairs)

    (set emacs-alist-sym (append pairs emacs-alist)) ))

;;
;; frame loading after first frame.
;;

(defvar grail-display-loaded nil
  "Boolean for if grail has configured the frame.")

(defun grail-load-display ( new-frame )
  "grail-load-display

   load the display after the first frame is created and
   the graphical related symbols have been defined.
  "
  (when (is-frame-gui new-frame)
    (when (not grail-display-loaded)
      ;; set best font
      (setq grail-font-best (grail-format-font (grail-select-best-font-family new-frame) grail-font-size new-frame))

      (when grail-font-best
        ;; set for all new frames
        (grail-merge-frame-parameters 'default-frame-alist `(font . ,grail-font-best))

        ;; set the best font for current frame
        (grail-set-best-font new-frame) )

      (let
        ((grail-frame new-frame))

        (grail-try-user-elisp "load-display") )

      (setq grail-display-loaded t) )

    (let
      ((grail-frame new-frame))

      (grail-try-user-elisp "configure-frame") ) ))

;;
;; font stuff
;;

(defvar grail-font-best nil)

(defun grail-select-best-font-family ( frame )
  (catch 'best-font
    (dolist (canidate grail-font-family)
      (when (member canidate (font-family-list frame))
        (throw 'best-font canidate)) )

    (car (font-family-list frame)) ))

(defun grail-format-font ( family size frame )
  (let
    (( font-spec (aref (font-info family frame) 1) ))
    (concat (replace-regexp-in-string
              "pixelsize=[[:digit:]]+"
              (format "pixelsize=%s" size) font-spec)
      ":spacing=m") ))

(defun grail-set-best-font ( frame )
  (set-frame-parameter frame 'font grail-font-best)

  (set-face-attribute 'default frame
    :font grail-font-best) )

;;
;; ELPA
;;

(defun load-elpa-when-installed ()
  "load-elpa-when-installed

   If the ELPA package management system http://tromey.com/elpa/ is installed,
   configure it for use, assuming a proper install by grail-install-elpa.

   t is returned if succesful, otherwise nil is returned.
  "
  (interactive)
  (if (< emacs-major-version 24)
    (grail-report-info "grail-load" "cannot load ELPA min Emacs ver. is 24" emacs-major-version)
    (progn
      (setq-default package-user-dir (grail-dir-always grail-dist-elpa))

      ;; ELPA is loaded so do the ugly parts and hook into package.el's guts
      ;; to pick up it's modifications to load-path

      (defadvice package-activate-1 (after grail-snoop/do-activate)
        (let
          ((snooped (car load-path))) ;; elpa always cons's the new path on the front.

          (when snooped
            (message "grail: snooped load-path update %s from package.el" snooped)
            (setq grail-elpa-load-path (cons snooped grail-elpa-load-path))
            (grail-update-load-path)) ))

      (ad-activate 'package-activate-1)

      (grail-fail
        "grail-load ELPA"
        "running package initialize to trigger a ELPA package load"
        (package-initialize)) ) ))

(provide 'grail-load)
