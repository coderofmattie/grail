;;----------------------------------------------------------------------
;; load.el
;; Primary Author: Mike Mattie
;;
;; This file extends load-path according to the configuration's
;; fs structure, and adapts to the host platform according to
;; the value of system-type.
;;----------------------------------------------------------------------

;; This code will assume a FS structure like this:
;;
;; $HOME/.emacs.d/            | all of emacs scribbles here by default so I treat
;;                              it like /var
;;  my-emacs-dir =
;;   $HOME/system/emacs       | keep my emacs configuration under version
;;                              control seperate from any emacs default path
;;                              so that odd files don't show up from emacs
;;                              sessions.
;;
;; load.el                    | entry point for emacs startup and phase #1
;;                              of the configuration.
;;
;; load-library.el            | library of functions essential to phase #1
;;
;; mattie.el                   | contains customization of emacs that is
;;                               robust, starts phase #2
;;
;; *.el                       | my libraries,code, and parts of the customization
;;                              that may fail.
;;
;; patches/                   | patches against distributed emacs files required
;;                              by my config.
;;
;; local/(*)                  | distributed files that have been locally modified
;;
;; elisp/(*)                  | Third party extensions that are not distributed by
;;                              emacs and not integrated through host package management.
;;                              This is the highest maintenance burden.

;; The config files are relocated to the $HOME/system/emacs so the config/code
;; under version control is not stomped on or cluttered by all the traffic
;; into the standard location: session and intra-session state.

;; * only system/emacs/{local,elisp} are placed in the load-path. the files in system/emacs
;;   are assumed to chain manually.

(defvar user-elisp-root
  (concat (getenv "HOME") "/system/emacs/")
  "The root of the user's elisp tree")

(defvar user-local-dir
  (concat my-emacs-dir "local/")
  "The directory containing the user's local modifications to emacs
   and elisp.

   user-local-emacs and user-local-elisp are the preferred
   variables for accessing user specific elisp paths.")

(defvar user-local-emacs
  (concat user-local-dir "emacs/")
  "The directory containing Emacs packages that over-ride the packages
   distributed with Emacs.")

(defvar user-local-elisp
  (concat user-local-dir "elisp/")
  "The directory containing Emacs libraries created and maintained by the
   user.")

(defvar user-dist-dir
  (concat my-emacs-dir "dist/"))

(defvar user-dist-elisp
  (concat user-dist-dir "elisp/")
  "The directory containing third-party elisp extensions of Emacs.")

;; here we need to load user-paths.el or some such thing so the user
;; can tune the paths.

;; first load a small file containing only the functions that are essential
;; to constructing the load path. Once the load-path, system adaptation,
;; site-file have been loaded we can be less paranoid.

(defun load-config ( path )
  "load a path relative to the configuration directory"
  (load-file (concat user-elisp-root path)))

(load-config "load-library.el")

(setq load-path
  (append
    ;; overide distributed elisp with local modifications by
    ;; inserting a "local" directory at the beginning of the
    ;; load list
    (cons user-local-emacs load-path)

    (filter-ls user-local-elisp t
        (type ?d)
        (!name "^\\."))

    (filter-ls user-dist-elisp t
      (type ?d)
      (!name "^\\.")) ))

;;----------------------------------------------------------------------
;; Host specific adaptation
;;
;; each host system has a site file that normalizes the platform
;; and extends the library search space for extra libraries it manages.
;;----------------------------------------------------------------------
(cond
  ;; Gentoo has a file that tunes emacs and loads the third party
  ;; components managed by the package manager.
  ((string-equal "gnu/linux" system-type)
    (load-file "/usr/share/emacs/site-lisp/site-gentoo.el"))

  ;; on darwin assume carbon-emacs.
  ((string-equal "darwin" system-type) (load-config "darwin.el"))
  )

(unless noninteractive
  ;; only loaded when there is an active terminal.
  (load-config "mattie.el"))

