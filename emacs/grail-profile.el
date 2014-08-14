;;;----------------------------------------------------------------------
;; grail-profile.el
;;----------------------------------------------------------------------
(require 'cl)
(require 'async-exec)
(require 'sync-exec)

(defvar grail-masked-profiles
  nil
  "List of grail profiles masked by the user.")

(defvar grail-requested-profiles
  nil
  "List of grail profiles requested by the user.")

(defvar grail-failed-profiles
  nil
  "List of grail profiles that failed to load.")

(defvar grail-loaded-profiles
  nil
  "List of grail profiles that have loaded.")

(defun grail-load-requested-profiles ()
  "grail-load-requested-profiles

   Load the profiles in the request list grail-requested-profiles.
  "
  (when grail-requested-profiles
    (let
      ((order-sorted (sort grail-requested-profiles (lambda ( a b )
                                                      (when (< (car a) (car b)) t)
                                                      )) ))
      (setq grail-requested-profiles nil)

      (mapc
        (lambda ( profile-order )
          (grail-report-info "grail-load-requested-profiles" "order" (car profile-order) "profile" (cdr profile-order))

          (mapc
            (lambda ( profile )
              (catch 'skip-profile
                (when (member profile grail-masked-profiles)
                  (grail-report-info "ignoring masked profile" profile)
                  (throw 'skip-profile t))

                (let
                  ((profile-path (concat grail-local-profiles profile) ))

                  (grail-report-info "grail-load-requested-profiles" "loading profile" profile-path)

                  (grail-recover
                    "grail-load-requested-profiles"
                    (format "attempting to load a profile %s " profile-path)

                    ;; recover is recording the fail
                    (push (cons (car profile-order) profile) grail-failed-profiles)

                    (grail-load-elisp profile-path)
                    (push profile grail-loaded-profiles) ) ) ))

            (cdr profile-order) ))
          order-sorted) )))

(defun grail-retry-failed-profiles ()
  "grail-retry-failed-profiles

   Retry the loading of any profiles that previously failed to load.
  "
  (interactive)

  (setq grail-requested-profiles grail-failed-profiles)
  (setq grail-failed-profiles nil)
  (grail-load-requested-profiles))

(defun use-grail-profiles ( order &rest request-list )
  "use-grail-groups: ORDER LIST

   request a list of string quoted groups to be loaded after the configuration
   files have been loaded.
  "
  (push (cons order request-list) grail-requested-profiles))


(defun mask-grail-profiles ( &rest request-list )
  "use-grail-groups: ORDER LIST

   mask profiles to not be loaded.
  "
  (setq grail-masked-profiles (append request-list grail-masked-profiles)))

;;
;; grail-installer tools
;;

(defconst grail-profile-buffer-name "*grail-loader*")

(defun grail-profile-buffer ()
  (get-buffer-create grail-profile-buffer-name))

(defun grail-show-buffer ()
  (pop-to-buffer (grail-profile-buffer)))


(defun grail-run-spec-builder ( rel-path commands )
  "grail-run-spec-builder REL-PATH COMMANDS

   REL-PATH is the relative path the installer will install
   into.

   COMMANDS is a list of the commands to run starting with
   the program and followed by any args.
  "
  (cons rel-path commands))

(defun grail-run-spec-result ( runner-spec )
  "grail-run-spec-result SPEC
   return the directory installed to relative to the installer
   directory.
  "
  (car runner-spec))

(defun grail-run-spec-command ( runner-spec )
  "grail-run-spec-command SPEC
   return the command and args of the runner spec.
  "
  (cdr runner-spec))

(defun grail-run-installer ( install-dir spec )
  "grail-run-installer INSTALL-DIR SPEC

   install into INSTALL-DIR using runner SPEC

   spec is built by grail-run-spec-builder.

   the path installed to relative to the INSTALL-DIR
   is returned.
  "
  (grail-report-info "grail-run-installer" "starting install with dir,command" install-dir (grail-run-spec-command spec))

  (with-current-buffer (grail-show-buffer)
    (let
      ((exit-status (sync-exec-list install-dir (grail-profile-buffer) (grail-run-spec-command spec)) ))

      (message "got here")
      (unless (equal 0 exit-status)
        (grail-signal-fail "grail-run-installer" "non-zero exit-status of installer"
          exit-status) )

      (message "got there")

      (concat install-dir "/" (grail-run-spec-result spec))

      (message "lots of fun %s" (concat install-dir "/" (grail-run-spec-result spec)))

      (concat install-dir "/" (grail-run-spec-result spec))
      )) )


;;
;; ELPA package
;;

(defun grail-package-installer ( module )
  (package-install module))

;;
;; file
;;

(defun grail-file-installer ( base-dir name url )
  "grail-file-installer BASE-DIR NAME URL

   install a file from URL in BASE-DIR with NAME
  "
  (let
    (( install-path  (concat base-dir "/" name) ))

    (with-temp-buffer
      (url-insert-file-contents url)
      (let
        ((buffer-file-coding-system 'no-conversion))
        (write-file install-path)))

    (grail-report-info "grail-file-installer" "completed install for" install-path url) ))

;;
;; git
;;

(defun grail-git-installer ( install-dir module url )
  (grail-run-installer install-dir
    (grail-run-spec-builder module (list "git" "clone" "-v" url module)) ) )

;;
;; cvs
;;

(defun grail-cvs-installer ( install-dir module url )
  (grail-run-installer install-dir
    (grail-run-spec-builder module (list "cvs" "-d" url "co" module)) ) )

;;
;; svn
;;

(defun grail-svn-installer ( install-dir module url )
  (grail-run-installer install-dir
    (grail-run-spec-builder module (list "svn" "checkout" url module)) ) )

;;
;; bzr
;;

(defun grail-bzr-installer ( install-dir module url )
  (grail-run-installer install-dir
    (grail-run-spec-builder module (list "bzr" "branch" url module)) ) )

;;
;; mercurial
;;

(defun grail-hg-installer ( install-dir module url )
  (grail-run-installer install-dir
    (grail-run-spec-builder module (list "hg" "clone" url module)) ) )

;;
;; grail-*-builder
;;

;; these are generic installer builders that can be tailored to a install dir

(defun grail-load ( package installer )
  (grail-report-info "grail-load" "attempting to load the profile: " (pp-to-string package))

  (unless (symbolp package)
    (grail-signal-fail "grail-load" (format "package \"%s\" is not a symbol" (symbol-name package))) )

  (grail-recover
    "grail-load"
    (format "attempting to load profile component %s" (symbol-name package))

    (grail-fail
      "grail-load retry"
      "attempting to recover from a failed profile load by installing"

      ;; try and force a unload ignoring errors hoping they are not blockers
      (grail-ignore
        "grail-load reset"
        "unload reset a failed profile"

        ;; force unload
        (unload-feature package t) )

      ;; try and install ignoring errors hoping they are not blockers
      (grail-ignore
        "grail-load install"
        "performing a install of a failed profile"

        (grail-report-info "grail-load install" "running installer for" package)

        (eval installer)

        (grail-report-info "grail-load install" "attempting reload for" package) )

      (grail-fail
        "grail-load retry load"
        "retrying to load a failed profile"

        (grail-update-load-path)
        (require package)

        (grail-report-info "grail-load" "profile loaded on install/retry" package) ) )

    (require package)
    (grail-report-info "grail-load" "profile loaded on the first try" package) ))


(defun grail-archive-specification ( specification )
  "grail-archive-specification SPEC

   SPEC is either a single value such as file|cvs, or a pair such
   as tar:bz2. When a pair is detected return it as a cons cell,
   or simply return the spec as given.
  "
  (let
    ((split-spec (string-match ":" specification)))

    (when split-spec
      (cons (substring specification 0 split-spec) (substring specification (match-end 0))) )))

(defun grail-define-installer ( package installer &optional url )
  "grail-define-installer PACKAGE INSTALLER NAME URL

   define a installer for a package NAME.
  "
  (grail-fail
    "grail-define-installer"
    (format "defining installer for package %s" package)

    (let*
      ((location nil)
       (base-dir (cond
                   ((string-equal "cvs"  installer) grail-dist-cvs)
                   ((string-equal "git"  installer) grail-dist-git)
                   ((string-equal "svn"  installer) grail-dist-svn)
                   ((string-equal "bzr"  installer) grail-dist-bzr)
                   ((string-equal "hg"   installer) grail-dist-hg)
                   ((string-equal "file" installer) (progn
                                                      (setq location (concat (symbol-name package) ".el"))
                                                      grail-dist-elisp))
                   ((string-equal "pkg"  installer) (progn
                                                      (setq location "*ignore*")
                                                      nil))

                   (t (grail-signal-fail "grail-define-installer"
                        "unable to construct installer for" (list package installer url))) )) )

      (unless location
        (setq location (symbol-name package)) )

      (unless url
        (unless (string-equal "pkg" installer)
          (grail-signal-fail "grail-define-installer"
            "only \"pkg\" type installers can have a empty url" (list package installer url)) ))

      (cond
        ((string-equal "pkg"  installer)  `(grail-pkg-installer ,package))

        ((string-equal "file" installer)  `(grail-file-installer ,base-dir ,location ,url))
        ((string-equal "cvs"  installer)  `(grail-cvs-installer ,base-dir ,location ,url))
        ((string-equal "git"  installer)  `(grail-git-installer ,base-dir ,location ,url))
        ((string-equal "svn"  installer)  `(grail-svn-installer ,base-dir ,location ,url))
        ((string-equal "bzr"  installer)  `(grail-bzr-installer ,base-dir ,location ,url))
        ((string-match "hg"   installer)  `(grail-hg-installer  ,base-dir ,location ,url)) ) )))

(defun grail-load-package ( package installer url )
  "grail-package-installer PACKAGE INSTALLER URL"
  (grail-load package (grail-define-installer package installer url)) )

(defun grail-fetch ( base-dir object installer )
  (let
    (( object-path (concat base-dir "/" object) ))

    (grail-recover
      "grail-fetch"
      (format "attempting to fetch profile resource %s" object)

      (grail-ignore
        "grail-fetch cleanup"
        "attempting to cleanup a bad fetch"

        (when (file-exists-p object-path)
          (if (file-directory-p object-path)
            (delete-directory object-path t nil)
            (delete-file object-path nil) )) )

      (unless (file-exists-p object-path)
        (grail-report-info "grail-fetch" "attempting to fetch profile resource: " object)

        (eval installer)

        (grail-report-info "grail-fetch" "retrieved profile resource %s" object)

        object-path) )))

(defun grail-define-fetch ( base-dir object installer url )
  "grail-define-installer PACKAGE INSTALLER NAME URL

   define a installer for a package NAME.
  "
  (grail-fail
    "grail-define-fetch"
    (format "defining fetch for resource %s" package)

    (cond
      ((string-equal "file" installer)  `(grail-file-installer ,base-dir ,object ,url))
      ((string-equal "cvs"  installer)  `(grail-cvs-installer ,base-dir ,object ,url))
      ((string-equal "git"  installer)  `(grail-git-installer ,base-dir ,object ,url))
      ((string-equal "svn"  installer)  `(grail-svn-installer ,base-dir ,object ,url))
      ((string-equal "bzr"  installer)  `(grail-bzr-installer ,base-dir ,object ,url))
      ((string-match "hg"   installer)  `(grail-hg-installer  ,base-dir ,object ,url)) ) ))

(defun grail-load-templates ( object installer url )
  (grail-fetch grail-dist-templates object
    (grail-define-fetch grail-dist-templates object installer url)) )

(defun grail-load-docs ( object installer url )
  (grail-fetch grail-dist-templates object
    (grail-define-fetch grail-dist-templates object installer url)) )

(provide 'grail-profile)
