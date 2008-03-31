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
;; local/(*)                  | elisp maintained by the user.
;;      emacs/                | local elisp that modifies or replaces packages distributed
;;                              with the mainline.
;;      elisp/                | elisp maintained by the user that complements the mainline.
;;
;;      patches/              | patches against distributed emacs files required
;;                              by my config.
;;      styles/               | modules that combine loading/deploying required packages
;;                              with configuration that blends those packages into a
;;                              harmonious "style" of using Emacs.

;; dist/(*)
;;     elisp/                 | elisp maintained and distributed by a Third Party.

;; The files user-elisp-root are not in the load-path and must be loaded with explicit paths.

(defvar user-elisp-root
  (or (getenv "USER_ELISP") (concat (getenv "HOME") "/system/emacs/"))
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

(defvar user-local-styles
  (concat user-local-dir "styles/")
  "The directory containing Emacs style modules.")

(defvar user-dist-dir
  (concat user-elisp-root "dist/"))

(defvar user-dist-elisp
  (concat user-dist-dir "elisp/")
  "The directory containing third-party elisp extensions of Emacs.")

;; FIXME: need setter functions when paths need to be re-computed as
;;        side-effects.

;;----------------------------------------------------------------------
;; define a robust loading command.
;;----------------------------------------------------------------------

(defun robust-load-elisp ( path )
  (condition-case nil
    (load std-path)
    (error (progn
             ;; duplicate the message to both *Messages* as a log
             ;; and to the *scratch* buffer where it is highly visible.
             (message "errors loading: %s\n" std-path)
             (with-current-buffer "*scratch*"
               (goto-char (point-max))
               (insert (format "; errors loading %s\n" std-path)))
             nil)) ))

(defun file-if-readable ( file )
  "if file is a readable path return file or nil"
  (if (file-readable-p file)
    file))

(defun load-elisp-if-exists ( path )
  (lexical-let
    ((std-path  (file-if-readable path)))

    (when std-path
      (robust-load-elisp std-path)) ))

(defun load-user-elisp ( path )
  (when path
    (load-elisp-if-exists (concat user-elisp-root path))))

;;----------------------------------------------------------------------
;; load the rest of the loader, and any customizations.
;;----------------------------------------------------------------------

(load-user-elisp "loader-fn.el")        ;; library used by the loader.
(load-user-elisp "loader-cfg.el")       ;; file for user to change paths

(setq load-filter-dot-dirs "^\\.")

(let
  ((extended-load-path
     (condition-case nil
       (apply 'append
         (seq-filter-nil
           ;; overide distributed elisp with local modifications by
           ;; inserting a "local" directory at the beginning of the
           ;; load list
           (if (file-accessible-directory-p user-local)
             (list user-local-emacs))

           load-path

           (if (file-accessible-directory-p user-local-elisp)
             (cons user-local-elisp
               (filter-ls user-local-elisp t
                 (type ?d)
                 (!name load-filter-dot-dirs))))

           (if (file-accessible-directory-p user-dist-elisp)
             (cons user-dist-elisp
               (filter-ls user-dist-elisp t
                 (type ?d)
                 (!name load-filter-dot-dirs))))))

       (error nil)) ))

  (if (and extended-load-path (listp extended-load-path))
    (setq load-path extended-load-path)
    (message "load.el: unable to form an extended load-path. check for problems loading loader-fn.el.") ))

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

(load-user-elisp "elisp.el")

(unless noninteractive
  ;; only loaded when there is an active terminal.
  (load-user-elisp "keys.el")
  (load-user-elisp "commands.el")
  (load-user-elisp "interface.el")

  (load-user-elisp "user.el")

  (if (window-system)
    (load-user-elisp "gui.el"))

  (load-requested-styles))

