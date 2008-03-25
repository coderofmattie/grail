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

(setq my-emacs-dir (concat (getenv "HOME") "/system/emacs/"))

(setq my-localized-dir (concat my-emacs-dir "local/"))
(setq my-extras-dir    (concat my-emacs-dir "elisp/"))

;; first load a small file containing only the functions that are essential
;; to constructing the load path. Once the load-path, system adaptation,
;; site-file have been loaded we can be less paranoid.

(defun load-config ( path )
  "load a path relative to the configuration directory"
  (load-file (concat my-emacs-dir path)))

(load-config "load-library.el")

(setq load-path
  (append
    ;; overide distributed elisp with local modifications by
    ;; inserting a "local" directory at the beginning of the
    ;; load list
    (cons my-localized-dir load-path)

    ;; add the extras and it's sub-directories to the end of the list.
    (cons my-extras-dir
      (filter-ls my-extras-dir t
        (type ?d)
        (!name "^\\."))) ))

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

;; load my advanced configuration
(load-config "mattie.el")
