;;;----------------------------------------------------------------------
;; grail.el
;; Primary Author: Mike Mattie
;; Copyright (C) 2008 Mike Mattie
;; License: LGPL-v3
;;----------------------------------------------------------------------

;; Grail is an attempt to seek the Holy Grail of Emacs Theory:
;; unification of all the various essential bits of elisp in a sane
;; deployment.
;;
;; Grail on a more basic level is a load-path hack for the Elisp coder
;; who can't leave anything alone.

;; the general design is a loader split into three parts:

;; 1. grail.el     - the first, and minimal stage of the loader. Small and simple code.
;; 2. grail-fn.el  - a larger library of functions used by the loader.
;; 3. grail-cfg.el - the first customization file loaded, for the opportunity to modify paths.

(defconst grail-release-version "0.0.4"
  "the release number of grail.el")

(defun dir-path-if-accessible ( path )
  "return the path if the directory is readable, otherwise nil"
  (if (and path (file-accessible-directory-p path))
    path
    nil))

(defun file-path-if-readable ( file )
  "return the path if the file is readable, otherwise nil"
  (if (file-readable-p file)
    file))

(defun grail-dup-error-to-scratch (error-message)
  "grail-dup-error-to-scratch ERROR-MESSAGE

  duplicate the ERROR-MESSAGE to both *Messages* as a log and to the
  *scratch* buffer as a comment where it is highly visible.
  "
  (message error-message)
  (with-current-buffer "*scratch*"
    (goto-char (point-max))
    (insert (format "; grail error! %s\n" error-message))) )

(defun format-signal-trap (signal-trap)
  "format-signal-trap list:SIGNAL-TRAP

   format SIGNAL-TRAP for use in error messages.
  "
  (format "(%s , \"%s\")" (symbol-name (car signal-trap)) (cadr signal-trap)) )

;;----------------------------------------------------------------------
;; define a robust methods of loading and evaluating elisp that trap
;; errors.
;;----------------------------------------------------------------------

(defun sanitize-load-path ( path )
  "sanitize-load-path PATH

   sanitize a load-path reducing redundant file separators to single
   separators. The sanitized PATH is returned.

   This situation: foo/bar/baz//  has bombed (load file) for me.
  "
  (replace-regexp-in-string "/+" "/" path))

(defun diagnostic-load-elisp-file ( path )
  "diagnostic-load-elisp-file PATH

   load a elisp file trapping any errors that occur. nil
   is returned for a successful load. If there are
   errors the signal is returned.
  "
  (condition-case error-trap
    (progn
      (load (sanitize-load-path path))
      nil)
    (error error-trap)))

(defun robust-load-elisp-file ( path )
  "robust-load-elisp-file PATH

   load a elisp file trapping any errors that occur. t is
   returned for a successful load, nil if there are errors. The
   caller can choose to process or ignore the errors.
  "
  (if (diagnostic-load-elisp-file path)
    nil
    t))

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

(defun load-elisp-if-exists ( path )
  "load-elisp-if-exists"
  (lexical-let
    ((accessible-path  (file-path-if-readable path)))

    (when accessible-path
      (robust-load-elisp-file accessible-path)) ))

(defun load-user-elisp ( path )
  "load-user-elisp PATH

   A fully guarded load that checks for a non-nil path, appends
   it to grail-elisp-root, and hands the resulting absolute path
   to load-elisp-if-exists.
  "
  (when path
    (load-elisp-if-exists (concat grail-elisp-root path))))

(defun load-user-elisp-file-with-error-reporting ( path )
  "load-elisp-file-with-error-reporting PATH

   load PATH relative to grail-elisp-root reporting any errors that occur.

   nil is returned on success, t on failure.
  "
  (let
    ((elisp-path  (file-path-if-readable (concat grail-elisp-root path))))

    (if elisp-path
      (let
        ((diagnostics (diagnostic-load-elisp-file elisp-path)))

        (if diagnostics
          (progn
            (grail-dup-error-to-scratch (format "grail: %s errors prevented %s from loading correctly"
                                          (format-signal-trap diagnostics)
                                          path))
            nil)
          t))
      nil) ))

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

  (let*
    ((filter-dot-dirs "^\\.")
     (extended-load-path
       (condition-case signal-trap
         (apply 'append
           (seq-filter-nil

             (if (file-accessible-directory-p grail-local-emacs)
               (list grail-local-emacs))

             grail-boot-load-path

             (if (file-accessible-directory-p grail-local-elisp)
               (cons grail-local-elisp
                 (filter-ls grail-local-elisp t
                   (type ?d)
                   (!name filter-dot-dirs))))

             grail-elpa-load-path

             (if (file-accessible-directory-p grail-dist-elisp)
               (cons grail-dist-elisp
                 (filter-ls grail-dist-elisp t
                   (type ?d)
                   (!name filter-dot-dirs))))))

         ;; if there is an error, trap and re-throw the error
         (error
           (error "grail-extend-load-path magic failed: %s. grail-fn.el has likely been humbled by recursion stack growth."
                  (cdr signal-trap))) )) )

    ;; minimally check that the extended-load-path, if it's ok AFAICT
    ;; then update load-path

    (if (and extended-load-path (listp extended-load-path))
      (setq load-path extended-load-path)
      (error "new extended-load-path is not a list !?! %s" (pp-to-string extended-load-path))) ))

(condition-case error-trap
  (progn
    (defvar grail-elisp-root
      (concat (or (dir-path-if-accessible (getenv "USER_ELISP"))
                  (dir-path-if-accessible (concat (getenv "HOME") "/system/emacs")))
        "/")
      "The root of the user's elisp tree")

    (unless grail-elisp-root
      (error "%s" "cannot access USER_ELISP directory !!"))

    (message "grail is loading USER_ELISP %s" grail-elisp-root)

    ;; This code will assume a FS structure like this:
    ;;
    ;; $HOME/.emacs.d/            | all of emacs scribbles here by default so I treat
    ;;                              it like /var
    ;;
    ;;  user-elisp-root           | all of my emacs customization and elisp. I keep
    ;;                              it outside of .emacs.d and under version control
    ;;                              so that emacs session state does not mix with
    ;;                              source which is very different in lifetime and
    ;;                              content management.

    ;; all of the following paths are relative to user-elisp-root. Files that
    ;; do not exist are silently ignored.

    ;; grail.el                   | entry point for emacs startup and phase #1
    ;;                              of the configuration.
    ;;
    ;; grail-fn.el                | library of functions essential to phase #1
    ;;
    ;; elisp.el                   | user elisp functions, should not be (interactive) only:
    ;;                            | loaded by --script

    ;; user.el                    | user customization of Emacs.

    ;; commands.el                | user commands only loaded in interactive Emacs
    ;; keys.el                    | user key-binding customization
    ;; interface.el               | modify the mainline Emacs interface, gui agnostic.
    ;; gui.el                     | only loaded with a window-system

    ;; linux.el                   | only loaded on gnu/linux.
    ;; darwin.el                  | only loaded on darwin.
    ;;
    ;; local/
    ;;      elisp/(*)             | elisp maintained by the user that complements the mainline.
    ;;
    ;;      emacs/                | local elisp that modifies or replaces packages/files distributed
    ;;                              with the mainline.
    ;;
    ;;      patches/              | patches against distributed emacs files required
    ;;                              by my config.
    ;;      groups/               | groups combine loading,deploying,and configuring one
    ;;                              or more packages into a cohesive configuration for
    ;;                              a set of related features.
    ;;
    ;; dist/
    ;;     elisp/(*)              | elisp maintained and distributed by a Third Party.
    ;;                              Usually this is for projects you contribute to where
    ;;                              you want to keep a VCS checkout.
    ;;     elpa/                  | elisp maintained by ELPA.


    ;; * The directory, and all of it's immediate sub-directories are added to
    ;;   load-path.

    ;; The files in the user-elisp-root directory are not added to the load-path and
    ;; should be loaded with the function (load-user-elisp FileName).

    (defvar grail-local-dir
      (concat grail-elisp-root "local/")
      "The directory containing the user's local modifications to emacs
       and elisp.

       grail-local-emacs and grail-local-elisp are the preferred
       variables for accessing user specific elisp paths.")

    (defvar grail-local-emacs
      (concat grail-local-dir "emacs/")
      "The directory containing Emacs packages that over-ride the packages
       distributed with Emacs.")

    (defvar grail-local-elisp
      (concat grail-local-dir "elisp/")
      "The directory containing Emacs libraries created and maintained by the
       user.")

    (defvar grail-local-groups
      (concat grail-local-dir "groups/")
      "The directory containing Emacs group modules.")

    (defvar grail-dist-dir
      (concat grail-elisp-root "dist/"))

    (defvar grail-dist-elisp
      (concat grail-dist-dir "elisp/")
      "The directory containing third-party elisp extensions of Emacs.")

    (defvar grail-dist-elpa
      (concat grail-dist-dir "elpa/")
      "ELPA managed third party elisp.")

    (defvar grail-elpa-load-path nil
      "The load-path extensions made by ELPA package activation")

    (defvar grail-boot-load-path load-path
      "The load-path as constructed by emacs before grail initialization")

    (defvar grail-state-path (concat (getenv "HOME") "/.emacs.d/")
      "The grail session state & persistent data directory which defaults to .emacs.d")

    (require 'cl)

    ;;----------------------------------------------------------------------
    ;; load the rest of the loader, and any customizations.
    ;;----------------------------------------------------------------------

    ;; the rest of the functions required.
    (let*
      ((stage-2-path (concat grail-elisp-root "grail-fn.el"))
       (stage-2-errors (diagnostic-load-elisp-file stage-2-path)))

      (when stage-2-errors
	(error "could not load %s , the second stage of grail. with %s errors. USER_ELISP does not point to a working GRAIL install" stage-2-path (format-signal-trap stage-2-errors))) )

    (load-user-elisp "grail-cfg.el")        ;; file for user to change paths

    (grail-extend-load-path)
    ;;----------------------------------------------------------------------
    ;; Host specific adaptation
    ;;
    ;; each host system has a site file that normalizes the platform
    ;; and extends the library search space for extra libraries it manages.
    ;;----------------------------------------------------------------------
    (load-user-elisp
      (cond
        ((string-equal "gnu/linux" system-type)  "linux.el")
        ((string-equal "darwin"    system-type)  "darwin.el")))

    (load-elpa-when-installed)

    ;; elisp loads the user's general elisp library.
    (load-user-elisp "elisp.el")

    ;; make sure there is a directory for session state and persistent data
    (unless (dir-path-if-accessible grail-state-path)
      (make-directory grail-state-path t))

    ;; Annoying Emacs.app 0.9-rc2 compat.
    (unless (functionp 'window-system)
      (defun window-system ()
	"grail.el replacement for window system function."
	window-system))

    (unless noninteractive
      ;; only loaded when there is an active terminal.
      (load-user-elisp-file-with-error-reporting "keys.el")
      (load-user-elisp-file-with-error-reporting "commands.el")
      (load-user-elisp-file-with-error-reporting "interface.el")

      (load-user-elisp-file-with-error-reporting "user.el")

      (if (window-system)
        (load-user-elisp-file-with-error-reporting "gui.el"))

      (grail-load-requested-groups))
    )
  (error
    (grail-dup-error-to-scratch
      (apply 'format "grail aborted ! %s" (cdr error-trap)))) )
