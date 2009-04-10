;;----------------------------------------------------------------------
;; alpha.el
;; Primary Author: Mike Mattie
;; Copyright: Mike Mattie (2007)
;; License: GPL v3.
;;----------------------------------------------------------------------

;;
;; stable-track  - canidate for inclusion in commands.el
;;

;; this can go in when it doesn't rely on a auto-overlay function.
(defun show-overlay-binding ( symbol )
  "show the overlay binding value of the symbol at the point"
  (interactive "Ssymbol? ")
  (pp (auto-overlay-local-binding symbol)))

;; a interactive command I still use. Just a quick way to pull up the
;; source in a read-only buffer. Once the completion is fixed to search
;; the load-path and use icicles for completion it can go into mattie.el.

(defun examine-library (library-name)
  ;; switch over to find-library and figure out how to make the buffer
  ;; automatically read-only.

  "examine the source of a library. Type the library name without
   any extension. If the library exists the source will be
   loaded"

  (interactive "F")
  (find-file-read-only (locate-library (concat library-name ".el"))))

;; This is a handy little function that allows you to localize
;; a distributed elisp source file. It assumes that the current
;; buffer is a distributed elisp file, and that localized-source-dir
;; points to a real directory.

;; This function needs to at least temporarily preserve version information
;; so that good diffs ( with ancestor information ) can be produced easily

(defun localize-distrib ()
  "localize a distributed lisp file by writing a copy of the file
   to a directory searched before the distributed lisp files"
  (interactive)

  (let
    ((new-name (file-name-nondirectory (buffer-file-name))))

    (let
      ((new-path
        (concat localized-source-dir
          (if (string-equal "gz" (file-name-extension new-name))
            (file-name-sans-extension new-name)
            (new-name)))))
      (if (yes-or-no-p (concat "localize distributed file " new-name " to " new-path))
        (write-file new-path)
        (message "aborted localizing distributed file")) )))

;; as soon as it works on darwin it can go to stable.el.
(defun copy-region-to-clipboard ()
  "copy the region to the clipboard"
  (interactive)
  (let
    ((x-select-enable-clipboard t))
    (x-select-text (filter-buffer-substring (region-beginning) (region-end)) t)
    ))

(defun find-child-directory-in-ancestor ( child parent )
  (catch 'done
    (unless (file-accessible-directory-p parent) (throw 'done nil))

    (mapc
      (lambda ( dir )
        (if (string-equal child dir)
          (throw 'done (concat parent "/" child))))
      (filter-ls parent nil
        (type ?d)
        (!path "^\\.\\.?")))

    (find-child-directory-in-ancestor
      child
      (lexical-let
        ((traverse (split-string parent "/" t)))

        (unless traverse (throw 'done nil))

        (prefix-strings "/" (strip-list-last traverse)))) ))

(defun line-at-point ()
  "return the entire line under the point as a string"
  (save-excursion
    (filter-buffer-substring
      (progn
        (beginning-of-line)
        (point))

      (progn
        (end-of-line)
        (point))
      nil
      t)))

;;----------------------------------------------------------------------
;; experimental - interesting
;;----------------------------------------------------------------------

;; these only work on X, when they work on the mac they can go into stable.el.

(defun maximize-frame ()
  "toggle maximization the current frame"
  (interactive)
  (cond
    ((eq 'x (window-system))

      (progn
        (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
          '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))

        (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
          '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
        ))
      ((message "window system %s is not supported by maximize" (symbol-name (window-system))))
    ))

(defun fullscreen ()
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
    '(2 "_NET_WM_STATE_FULLSCREEN" 0)))

(defun elisp-in-path ( path path-type )
  "return a list of elisp files in the path"

  (filter-ls path path-type
    (type ?-)
    (path "\\.el$")))
