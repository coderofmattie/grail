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
;;
;;  user-elisp-root           | all of my emacs customization and elisp. I keep
;;                              it outside of .emacs.d and under version control
;;                              so that emacs session state does not mix with
;;                              source which is very different in lifetime and
;;                              content management.
;;
;; load.el                    | entry point for emacs startup and phase #1
;;                              of the configuration.
;;
;; load-library.el            | library of functions essential to phase #1
;;
;; mattie.el                   | contains customization of emacs that is
;;                               robust, starts phase #2
;;
;;
;; local/(*)                  | elisp maintained by the user.
;;      emacs/                | local elisp that modifies or replaces packages distributed
;;                              with the mainline.
;;      elisp/                | elisp maintained by the user that complements the mainline.
;;
;;      patches/              | patches against distributed emacs files required
;;                              by my config.

;; dist/(*)
;;     elisp/                 | elisp maintained and distributed by a Third Party.

;; The files user-elisp-root are not in the load-path and must be loaded with explicit paths.

(defvar user-elisp-root
  (concat (getenv "HOME") "/system/emacs/")
  "The root of the user's elisp tree")

(defvar user-local-dir
  (concat user-elisp-root "local/")
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
  (concat user-elisp-root "dist/"))

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

    (cons user-local-elisp
      (filter-ls user-local-elisp t
        (type ?d)
        (!name "^\\.")))

    (cons user-dist-elisp
      (filter-ls user-dist-elisp t
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

(unless noninteractive
  ;; only loaded when there is an active terminal.
  (load-config "mattie.el"))

