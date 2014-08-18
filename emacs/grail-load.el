;;;----------------------------------------------------------------------
;; grail-load.el
;;----------------------------------------------------------------------
(require 'cl)

;;
;; grail-match - return a list of paths that match a given criteria
;;

(defun grail-match-path-attributes ( path filter-name filter-value )
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

(defmacro grail-match-path-logic ( path filter &rest body )
  `(if (eq 'not (car ,filter))
     (when (grail-match-path-attributes ,path (car (cdr ,filter)) (car (cdr (cdr ,filter))))
       ,@body)
     (unless (grail-match-path-attributes ,path (car ,filter) (car (cdr ,filter)))
       ,@body) ) )

(defun grail-match-path ( directory filters )
  (let
    ((contents (directory-files-and-attributes directory ))
     (filtered nil))

    (mapc (lambda ( path )
            (catch 'eliminate
              (mapc (lambda ( filter )
                      (grail-match-path-logic path filter
                        (throw 'eliminate t)))
                filters)

              (setq filtered (cons path filtered)) ))
      contents)

    (mapcar (lambda ( path-with-attributes )
              (concat directory "/" (car path-with-attributes)) )
      filtered) ))

;;
;; grail-match - return a list of paths that match a given criteria
;;

(defun grail-dirs-filter ( dir-list )
  (cond
    ((eq nil dir-list)   '() )
    ((listp dir-list)    (let
                           ((exists-list nil))

                           (mapc (lambda ( dir )
                                   (when (grail-dir-if-ok dir)
                                     (setq exists-list (cons dir exists-list))) )
                             dir-list)

                           exists-list) )
    ((stringp dir-list)  (if (grail-dir-if-ok dir-list) (list dir-list) '()) )
    (t                   (grail-signal-fail "grail-dirs-filter" "checking directory accessibility" "cannot read directory") ) ))

(defun grail-dirs ( path )
  (if (and path (grail-dir-if-ok path))
    (grail-dirs-filter
      (grail-match-path path
        '(("type" "dir")
           (not "path" "^\.\.?$")) ))
    '()) )

(defun grail-recurse-load-path ( dir )
  (when (grail-dir-if-ok dir)
    (let
      (( elisp-files (grail-match-path dir
                       '(("type" "file")
                         ("path" ".*\.elc?$")) ) )
        ( elisp-dirs nil ))

      (when elisp-files
        (setq elisp-dirs (cons dir elisp-dirs)))

      (mapc
        (lambda ( dir )
          (let
            ((next-level (grail-recurse-load-path dir) ))

            (when next-level
                  (setq elisp-dirs (append next-level elisp-dirs)) ) ) )
        (grail-dirs dir) )
      elisp-dirs) ))

(defmacro grail-new-load-path ( &rest body )
  `(let
     (( new-load-path grail-platform-load-path )
      ( search-results nil ))

     ,@(mapcar
        (lambda ( path )
          `(progn
             (setq search-results
               (if (listp ,path)
                 (grail-dirs-filter ,path)
                 (grail-recurse-load-path ,path)))

             (when search-results
               (setq new-load-path (append search-results new-load-path))) ))
         body)

     new-load-path))

;;
;; keep a table of all the dirs where we install so we can later
;; lookup by package a path to get misc data files out and see
;; if we have installed a package vs. it being built-in to emacs.
;;

(defvar grail-package-path-table (make-hash-table :test 'equal))

(puthash "elisp" grail-dist-elisp grail-package-path-table)

(defun grail-update-package-paths ( dir-list )
  (mapc
    (lambda ( dir )
      (when (file-accessible-directory-p dir)
        (let
          (( sub-dirs
             (grail-match-path dir
               '(("type" "dir")
                 (not "path" "^\.\.?$"))) ))

          (when sub-dirs
            (mapc
              (lambda ( sub-dir )
                (let
                  (( dir-name (file-name-nondirectory sub-dir) ))

                  (unless (gethash dir-name grail-package-path-table)
                    (puthash dir-name sub-dir grail-package-path-table)) ))
              sub-dirs)) )))
      dir-list))

(defun grail-update-load-path ()
  "grail-update-load-path

   update the load path. all of the paths listed are scanned for sub-dirs and
   added. everything is pre-pended to grail-platform-load-path.
  "
  (let
    ((new-load-path
       (grail-new-load-path

         ;;
         ;; ELPA kinda stale
         ;;
         grail-elpa-load-path

         ;;
         ;; my code smells like roses so load first.
         ;;

         grail-local-elisp

         ;;
         ;; fresh elisp
         ;;

         grail-dist-elisp

         ;;
         ;; fresh version control
         ;;

         grail-dist-cvs
         grail-dist-git
         grail-dist-bzr
         grail-dist-hg
         grail-dist-svn )))

    (when new-load-path
      (setq load-path new-load-path))

    (grail-update-package-paths (list grail-dist-cvs grail-dist-git grail-dist-bzr grail-dist-hg grail-dist-svn)) ))

(defun grail-package-resource-internal ( pkg-dir resource )
  (catch 'found
    (unless (grail-dir-if-ok pkg-dir)
      (throw 'found nil) )

    (let
      (( found-paths
         (grail-match-path pkg-dir
           `(("path" ,resource))) ))

      (when found-paths
        (throw 'found found-paths))

      (let
        (( sub-dirs
           (grail-match-path pkg-dir
             '(("type" "dir")
               (not "path" "^\.\.?$"))) ))

        (unless sub-dirs
          (throw 'found nil) )

        (mapc
          (lambda ( dir )
            (let
              (( next-level (grail-package-resource-internal dir resource) ))

              (when next-level
                (throw 'found next-level)) ))
          sub-dirs))
      nil)) )

(defun grail-package-resource ( package resource )
  (let
    (( pkg-lookup (or (grail-package-resource-internal (gethash package grail-package-path-table) resource)
                      (grail-package-resource-internal (gethash "elisp" grail-package-path-table) resource) ) ))

    (when pkg-lookup
      (car pkg-lookup) ) ))

(defun grail-install-sentinel ( package path )
  (grail-package-resource package path) )

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
