;;----------------------------------------------------------------------
;; beta.el
;;----------------------------------------------------------------------

(require 'cm-string)
(require 'cm-list)

(eval-when-compile
;;  this works when compiled and eliminates a unkown symbol warning, but when source loaded
;;  it croaks with cannot load file.
;;  (load "elisp")
  (require 'cl)
  (require 'grail-fn))

;;
;; stable-track  - canidate for inclusion in commands.el
;;

;; this can go in when it doesn't rely on a auto-overlay function.

;; (defun show-overlay-binding ( symbol )
;;   "show the overlay binding value of the symbol at the point"
;;   (interactive "Ssymbol? ")
;;   (pp (auto-overlay-local-binding symbol)))

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
          '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0)) ))

    ((eq 'w32 (window-system))
      (w32-send-sys-command 61488))

    ((message "window system %s is not supported by maximize" (symbol-name (window-system)))) ))

(defun fullscreen ()
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
    '(2 "_NET_WM_STATE_FULLSCREEN" 0)))

(defun elisp-in-path ( path path-type )
  "return a list of elisp files in the path"

  (filter-ls path path-type
    (type ?-)
    (path "\\.el$")))
