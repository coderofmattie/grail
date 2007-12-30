;;----------------------------------------------------------------------
;; adapt.el
;; Primary Author: Mike Mattie
;;
;; This file does two important things at boot. First it disables
;; customize. Second it performs phase #1 of the initialization
;; which is host adaptation.
;;----------------------------------------------------------------------

;; disable customization, automatic persistence of configuration changes.
;; I personally don't like customize as I prefer emacs to start with
;; a state I have personally defined and reviewed.

;; this line is a nasty way of disabling customize, simply specify the
;; customize file as /dev/null.

(setq custom-file "/dev/null")

;;======================================================================
;;         Phase 1: Library loading and host init/normalization
;;======================================================================

;;----------------------------------------------------------------------
;; extend the library search space with local changes and third pary
;; extensions
;;----------------------------------------------------------------------

;; This code will assume a FS structure like this:
;;
;; $HOME/.emacs.d/            | all of emacs scribbles here by default so I treat
;;                              it like /var
;;
;; $HOME/system/emacs         | root of my emacs extension tree.

;; $HOME/system/emacs/emacs.el| entry point for emacs initialization
;; $HOME/system/emacs/*.el    | other libraries.

;; $HOME/system/emacs/local   | distributed files that have been locally modified

;; $HOME/system/emacs/elisp   | Third party extensions that are not distributed by
;;                              emacs and not integrated through host package management.
;;                              This is the highest maintenance burden.
;; $HOME/system/emacs/patches | patches against upstream

;; The config files are relocated to the $HOME/system/emacs so the config/code
;; under version control is not stomped on or cluttered by all the traffic
;; into the standard location: session and intra-session state.

;; only system/emacs/{local,elisp} are placed in the path. the files in system/emacs
;; are assumed to chain manually.

(setq my-emacs-dir (concat (getenv "HOME") "/system/emacs/"))

;; first load a small file containing only the functions that are essential
;; to constructing the load path. Once the load-path, system adaptation,
;; site-file have been loaded we can be less paranoid.

(load-file (concat my-emacs-dir "mattie-boot.el"))

(setq my-localized-dir (concat my-emacs-dir "local/"))
(setq my-extras-dir    (concat my-emacs-dir "elisp/"))

(setq load-path
  (append
    ;; overide distributed elisp with local modifications by
    ;; inserting a "local" directory at the beginning of the
    ;; load list
    (cons my-localized-dir load-path)

    ;; add the extras to the end of the list.
    (cons my-extras-dir (subdirs-of-path my-extras-dir t))
    ))

;;----------------------------------------------------------------------
;; Host specific adapation
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
  ((string-equal "darwin" system-type)
    (load-file (concat my-emacs-dir "darwin.el")))
  )

;; load the real emacs file. It would be nice if errors were trapped or
;; ignored.
(load-file (concat my-emacs-dir "emacs.el"))
