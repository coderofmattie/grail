
;;----------------------------------------------------------------------
;; grail.el
;;----------------------------------------------------------------------
(require 'cl)
(require 'subr-x)

;; Grail loads an .emacs configuration in a robust, modular, and mode
;; aware manner.

;; The user has the opportunity to split their emacs configuration
;; into seperate files making it much easier to maintain. If the elisp
;; is sorted into the files treated as special, where grail loads them
;; directly by name, then the configuration will work properly when
;; emacs in started in different modes:

;; * batch mode    - non-interactive library functions only
;; * tty           - a console frame
;; * gui           - a graphical frame
;; * deamon        - a headless emacs server

;; The error trapping in grail catches errors in both errors in grail
;; itself, and in the configuration. This allows Emacs to start
;; containing the errors and reporting them. This is vital to using
;; Emacs to fix such errors :) and avoiding the dreaded --debug-init.

;; The second level of features involve managing load-path and third
;; party code not distributed with emacs itself.

;; It is very common for load-path to evolve, or devolve :) in a
;; hackish way until it is difficult to maintain. The tree structure
;; for a configuration defined by grail is used to construct a
;; load-path that scales from simple configuration to ambitious elisp
;; hacking.

;; There wealth of third party emacs packages provide powerful
;; functionality, but they are a burden to maintain, and especially to
;; replicate across multiple machines.

;; The primary mechanism for managing third party elisp is installing
;; and activating ELPA, the package management system hosted at
;; http://tromey.com/elpa/.  For those packages not hosted by ELPA
;; grail can install them in a limited way.

;; To collalesce a variety of packages into a feature group Grail
;; provides the "profile" configuration files. In a profile file
;; third party elisp is managed with installation and configuration.

;; In a group when elisp packages are missing an install function and
;; a initialization function are generated, with calls to those
;; functions inserted into the scratch buffer. The user can uncomment
;; the functions and execute them to replace the missing peices and
;; execute the configuration bits that depend on them.

;; ---> Starting with Grail.

;; The README.grail file provides installation instructions a more
;; detailed description of the file and directory structure that is
;; significant to Grail.

(defconst grail-release-version "0.3.1"
  "the release number of grail.el")

(defconst grail-maintainer-email "codermattie@gmail.com"
  "The maintainer's e-mail address")

(defconst grail-project-url "http://www.emacswiki.org/emacs/Grail"
  "the project page for Grail")

;;
;; grail error handling
;;

(defun grail-format-error ( information )
  (string-join
    (mapcar
      (lambda ( info )
        (cond
          ((not info)     "no information")
          ((stringp info) info)
          (t              (pp-to-string info)) ) )
      information)
    " -> ") )

(defun grail-report ( type component message &optional info )
  (with-current-buffer "*scratch*"
    (let
      ((report-start (point))
       (report (format "Grail %s! (%s) while \"%s\": %s"
                 type
                 component
                 message
                 (cond
                   ((not info)      "no information")
                   ((stringp info)  info)
                   ((listp info)    (grail-format-error info))
                   (t               (pp-to-string info)) )) ))

      ;; put in both messages and the scratch buffer
      (message report)
      (insert (concat "; " report)) )

    (insert "\n\n") ))

(defun grail-report-fail ( component message &optional info )
  (grail-report "Failure" component message info))

(defun grail-report-error ( component message &optional info )
  (grail-report "Error/ignored" component message info))

(defun grail-report-info ( component &rest info )
  (message "Grail Info (%s) %s" component (string-join (mapcar 'pp-to-string info) " ") ))

(define-error 'grail-fail "grail failure")
(defconst grail-abort nil)

(defun grail-mk-signal ( &rest other )
  (let
    (( signal-data '() ))

    (mapc
      (lambda (x)
        (when x
          (setq signal-data (cons x signal-data)) ))
      other)

    signal-data))

(defun grail-signal-fail ( fail-where fail-what &optional info )
  (grail-report-fail fail-where fail-what info)
  (signal 'grail-fail (grail-mk-signal (concat fail-where " failed")) ))

(defun grail-signal-error ( fail-where fail-what &optional info )
  (grail-report-error fail-where fail-what info)
  (signal 'grail-fail (grail-mk-signal (concat fail-where " failed")) ))

(defun grail-signal-abort ( fail-where fail-what &optional info)
  "dual use. reports the error given and throws abort."
  (condition-case trap-error
    (progn
      (grail-report-error fail-where fail-what info)
      (throw 'grail-abort 'grail-abort) )

    ('error
      (grail-signal-fail fail-where "could not throw a grail-fail abort" trap-error) ) ))

(defmacro grail-fail ( where what &rest body )
  `(condition-case trap-error
     (let
       ((trap-abort (catch 'grail-abort ,@body)))

       (when (equal trap-abort 'grail-abort)
         (grail-signal-fail ,where ,what trap-abort))

       trap-abort)

     ('grail-fail
       (grail-signal-fail ,where ,what trap-error) )
     ('error
       (grail-signal-fail ,where ,what trap-error) ) ))

(defmacro grail-abort ( where what &rest body )
  `(condition-case trap-error
     (progn ,@body)

     ('grail-fail
       (grail-signal-abort ,where ,what trap-error) )
     ('error
       (grail-signal-abort ,where ,what trap-error) ) ))

(defmacro grail-recover ( where what recover &rest body )
  `(condition-case trap-error
     (progn ,@body)

     ('grail-fail
       (unless ,recover
         (grail-signal-abort ,where ,what trap-error)) )
     ('error
       (unless ,recover
         (grail-signal-abort ,where ,what trap-error)) ) ))

(defmacro grail-ignore ( where what &rest body )
  `(condition-case trap-error
     (progn ,@body)

     ('grail-fail
       (grail-report-error ,where ,what trap-error)
       nil )

     ('error
       (grail-report-error ,where ,what trap-error)
       nil) ) )

;;
;; path handling
;;

(defun grail-file-if-ok ( path )
  "return the path if the file is readable, otherwise nil"
  (if (and path (file-readable-p path))
    (expand-file-name path)) )

(defun grail-dir-if-ok ( path )
  "return the path if the directory is readable, otherwise nil"

  (if (and path (file-accessible-directory-p path))
    (expand-file-name path)) )

(defun grail-dir-always ( path )
  "grail-dir-always

   If the directory PATH does not already exist then create it.
   return the path of the directory or nil.
  "
  (if (grail-dir-if-ok path)
    (expand-file-name path)
    (progn
      (make-directory path t)
      (expand-file-name path)) ))

(defun grail-elisp-path ( path )
  (when path
    (catch 'found-path
      (mapc
        (lambda (p)
          (let
            (( full-path (concat path p) ))

            (if (file-readable-p full-path)
              (throw 'found-path full-path)) ))
        '("" ".elc" ".el") )
      nil)))

(defun grail-user-path ( path )
  (grail-elisp-path (concat grail-elisp-root "/" path)) )

;;
;; elisp loading
;;

(defun grail-load-elisp ( path )
  "grail-load-elisp PATH

   load a required file. If the file is not found or a loading error occurs
   a grail-fail signal is raised.
  "

  (let
    (( full-path (grail-elisp-path path) ))

    (unless full-path
      (grail-signal-fail "grail-load-elisp" (format "file \"%s\" not found" path)))

    (grail-fail
      "grail-load-elisp"
      (format "loading a required elisp file: %s @ %s" path full-path)

      (load (expand-file-name full-path))

      (grail-report-info "grail-load-elisp" "loaded path: " full-path) ) ))

(defun grail-load-user-elisp ( path )
  (grail-load-elisp (grail-user-path path)) )

(defun grail-try-elisp ( path )
  "grail-try-elisp PATH

   load a elisp file PATH if it exists otherwise ignore a non-existent file.
  "
  (let
    (( full-path (grail-elisp-path path) ))

    (when full-path
      (grail-ignore
        "grail-try-elisp"
        (format "loading an optional elisp file: %s @ %s" path full-path)

        (load (expand-file-name full-path))
        (grail-report-info "grail-try-elisp" "loaded path: " full-path) )) ))

(defun grail-try-user-elisp ( path )
  (grail-try-elisp (grail-user-path path)) )

;;
;; Loading Entry Point
;;

(grail-ignore
  "Grail Core"
  "Grail Loading...."

  (grail-fail
    "grail elisp-root"
    "Finding elisp-root"

    ;;
    ;; establish the root of the USER_ELISP configuration tree.
    ;;

    (defvar grail-elisp-root
      (expand-file-name (concat (getenv "USER_ELISP") "/"))
      "The root of the user's elisp tree")

    (grail-report-info "grail" "checking elisp-root" grail-elisp-root)

    ;; abort the rest of grail if the USER_ELISP tree cannot be found.
    (unless (grail-dir-if-ok grail-elisp-root)
      (grail-signal-fail "grail" "checking directory accessibility" "cannot read directory")) )

  (grail-fail
    "grail elisp-root"
    "loading grail loader"

    (grail-load-user-elisp "grail-load")
    (grail-report-info "grail" "loader loaded" grail-elisp-root) )

  (defconst grail-local-dir
    (concat grail-elisp-root "local/")
    "The directory containing the user\'s local modifications to emacs
     and elisp.

     grail-local-emacs and grail-local-elisp are the preferred
     variables for accessing user specific elisp paths.")

  (defconst grail-local-profiles (concat grail-local-dir "profiles/")
    "The directory containing Grail profiles modules.")

  (defconst grail-local-emacs
    (concat grail-local-dir "/emacs/")
    "The directory containing Emacs packages that over-ride the packages
     distributed with Emacs.")

  (defconst grail-local-elisp
    (concat grail-local-dir "/elisp/" )
    "The directory containing Emacs libraries created and maintained by the
     user.")

  (defconst grail-local-templates
    (concat grail-local-dir "/templates/" )
    "local templates maintained by the user.")

  (defconst grail-dist-dir
    (concat grail-elisp-root "/dist/")
    "The directory for managing distributed packages")

  (defconst grail-dist-elpa
    (concat grail-dist-dir "elpa/")
    "ELPA managed third party elisp.")

  (defconst grail-dist-archive
    (concat grail-dist-dir "/archive/")
    "The directory for managing distributed packages")

  (defconst grail-dist-docs
    (concat grail-dist-dir "/docs/")
    "the directory containing third party docs")

  (defconst grail-dist-templates
    (concat grail-dist-dir "/templates/")
    "the directory containing third party docs")

  (defconst grail-elisp-dir "elisp/")
  (defconst grail-dist-elisp
    (concat grail-dist-dir "/" grail-elisp-dir)
    "The directory containing third-party elisp extensions of Emacs.")

  (defconst grail-cvs-dir "cvs/")
  (defconst grail-dist-cvs
    (concat grail-dist-dir "/" grail-cvs-dir)
    "cvs version control managed third party elisp")

  (defconst grail-bzr-dir "bzr/")
  (defconst grail-dist-bzr
    (concat grail-dist-dir "/" grail-bzr-dir)
    "bzr version control managed third party elisp")

  (defconst grail-git-dir "git/")
  (defconst grail-dist-git
    (concat grail-dist-dir "/" grail-git-dir)
    "git version control managed third party elisp")

  (defconst grail-svn-dir "svn/")
  (defconst grail-dist-svn
    (concat grail-dist-dir "/" grail-svn-dir)
    "subversion version control managed third party elisp")

  (defconst grail-hg-dir "hg/")
  (defconst grail-dist-hg
    (concat grail-dist-dir "/" grail-hg-dir)
    "mercurial version control managed third party elisp")

  (defvar grail-elpa-load-path nil
    "The load-path extensions made by ELPA package activation")

  (defvar grail-boot-load-path load-path
    "The load-path as constructed by emacs before grail initialization")

  (defvar grail-platform-load-path nil
    "The load-path after the platform files have been loaded.")

  (defconst grail-state-path (concat (getenv "HOME") "/.emacs.d/")
    "The grail session state & persistent data directory which defaults to .emacs.d")

  (defconst grail-settings-file
    (concat grail-state-path "/" "customize-settings.el")
    "The file where Emacs writes settings and customize data")

  (defconst grail-interpreters-path (concat grail-elisp-root "/interpreters/")
    "the path to the grail interpreters directory for interpreter files.")

  (defconst grail-server-state (concat grail-state-path "/server/")
    "the path to the grail interpreters directory for interpreter files.")

  (defvar grail-font-family nil
    "a list of preferred font families")

  (defvar grail-font-size 24
    "preferred size of fonts")

  (grail-ignore
    "emacs persistent state"
    "make state dir, redirect user-init-file and custom-file variables to grail-settings-file"

    ;; make sure there is a directory for session state and persistent data
    (grail-dir-always grail-state-path)

    ;; the user-init-file _must_ be changed otherwise emacs will
    ;; scribble all over grail which is not OK.

    ;; The customize file path also needs to be set so that
    ;; customize writes settings to a data-file rather than
    ;; appending them to code.

    (setq user-init-file
      (setq custom-file
        grail-settings-file))

    ;; Load the user's settings if any. Do it early so that this
    ;; stuff can be over-ridden in their config files.  The priority
    ;; of customize settings is a toss-up, but it only comes into
    ;; play when the user advances beyond relying on customize, and
    ;; by then the priority is sensible.
    (grail-try-user-elisp grail-settings-file) )

  ;;----------------------------------------------------------------------
  ;; Host specific adaptation
  ;;
  ;; Each host system has a file that normalizes the platform
  ;; and extends the library search space for extra libraries the system
  ;; manages.
  ;;
  ;; these files and platform specific customization are loaded by
  ;; platform here.
  ;;----------------------------------------------------------------------

  ;; save the state of load-path after the platform file if any has
  ;; been loaded.
  (setq grail-platform-load-path load-path)

  (grail-recover
    "load standard elisp"
    "loading all stock elisp and ELPA except Grail Profiles"
    (setq load-path grail-platform-load-path)

    (grail-update-load-path)

    (load-elpa-when-installed) )

  ;; done after the load-path update so grail-profile can use
  ;; libraries in local/elisp

  (grail-fail
    "grail profile"
    "loading grail profiles"

    (grail-load-user-elisp "grail-profile") )

  (grail-ignore
    "load system elisp"
    "Loading the OS specific elisp."

    (grail-try-user-elisp
      (cond
        ((string-equal "gnu/linux" system-type)  "systems/linux")
        ((string-equal "darwin"    system-type)  "systems/darwin")
        ((string-equal "windows"   system-type)  "systems/windows")))

    (grail-try-user-elisp
      (concat "hosts/" (system-name)))

    (grail-try-user-elisp
      (concat "users/" (user-login-name))) )

  (grail-ignore
    "Emacs Server"
    "configuration before use"

    ;; make sure there is a directory for server data
    (grail-dir-always grail-server-state)

    (set-file-modes grail-server-state
      (file-modes-symbolic-to-number "go-rwx" (file-modes grail-server-state)))

    (require 'server)

    (setq
      server-use-tcp t
      server-auth-dir grail-server-state) )

  (grail-ignore
    "user-elisp loading"
    "final loading with of user elisp"

    (grail-try-user-elisp "elisp")

    ;; only loaded when there is an active terminal.
    (grail-try-user-elisp "interface")

    (grail-try-user-elisp "user")

    (grail-try-user-elisp "programming")

    ;; load commands and keys last so they can use definitions from user.el
    ;; and friends.
    (grail-try-user-elisp "commands")
    (grail-try-user-elisp "keys") )

  (grail-ignore
    "interface loading"
    "configuring for daemon or application"

    (if (daemonp)
      (grail-ignore
        "daemon mode"
        "setting hooks to defer configuring graphical properties"

        (add-hook 'before-make-frame-hook 'grail-configure-display t)
        (add-hook 'after-make-frame-functions 'grail-load-display t) )
      (grail-ignore
        "application mode"
        "running graphical config immediately"

        (server-start)

        (grail-configure-display)
        (grail-load-display (window-frame))

        ;; do this so the per frame stuff loads for all frames
        (add-hook 'after-make-frame-functions 'grail-load-display t) ) ))

  (grail-ignore
    "Grail Profiles"
    "loading Grail Profiles"

    (grail-load-requested-profiles) ) )
