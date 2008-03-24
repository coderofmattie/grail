;;----------------------------------------------------------------------
;; alpha.el
;; Primary Author: Mike Mattie
;; Copyright: Mike Mattie (2007)
;; License: GPL v3.
;;----------------------------------------------------------------------

;;----------------------------------------------------------------------
;; stable-track  - canidate for inclusion in mattie.el
;;----------------------------------------------------------------------

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
  (find-file-read-only (locate-library (concat library-name ".el")))
  )

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
        (message "aborted localizing distributed file"))
    )))

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
;; unterminated lists experiments.
;;----------------------------------------------------------------------

(defun join-cons ( a b )
  "like cons but joins as a list instead of nesting"
  (let
    ((new-a (if (cdr a) (cons a nil) a))
     (new-b (if (cdr b) (cons b nil) b)) )
    (setcdr new-a new-b)
    new-a))

(defun terminate-sequence ( &rest args )
  "terminate sequence takes a all types concatenates into a list properly handling unterminated sequences"
  (lexical-let
    ((terminated nil))

    (dolist (arg (reverse args))
      (if (and (consp arg) (not (eq 'quote (car arg))))
        (lexical-let
          ((reverse-stack nil)
            (sequence arg))

          (while (consp sequence)
            (push (car sequence) reverse-stack)
            (setq sequence (cdr sequence)))

          (if sequence (push sequence reverse-stack))
          (setq terminated (append (reverse reverse-stack) terminated)))

        (setq terminated (cons arg terminated)) ))
    terminated))

(defun terminated-list-p ( list )
  "return true only if the list is nil terminated"
  (if (consp list)
    (lexical-let
      ((element (cdr list)))

      (while (consp element)
        (setq element (cdr element)))

      (eq nil element)) ))

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

;;----------------------------------------------------------------------
;; deploy experiments.
;;----------------------------------------------------------------------

(defun deploy-url-elisp ( url file )
  "deploy the elisp on the host via url installing in the extras path"
  (with-temp-buffer
    ;; download without modifying the buffer-name
    (and
      (condition-case nil
        (url-insert-file-contents url nil)
        (error (progn
                 (message "download of %s failed" file)
                 nil)))

      ;; write out to the appropriate file.
      (write-file (concat my-extras-dir file ".el")))
      ))

(defun deploy-query-inquisitio-search ( package )
  (list "-s" package)
  )

;; within this window can I make hyperlinks where the hyperlink triggers an install
;; method ?

;; can I extract available versions ? , installed versions ? , matching packages ?
(defun deploy-paludis ( package )
  "search for an Emacs package with Paludis's inquisitio search tool."
  (interactive "MPackage? ")

  (lexical-let
    ((search-buffer (generate-new-buffer "deploy-paludis")))

    (with-current-buffer search-buffer
      (unless (= 0 (apply 'call-process "inquisitio"     ;; search program
                     nil                                 ;; no stdin
                     (list (current-buffer) nil)         ;; discard stderr , stdout -> current-buffer
                     nil                                 ;; don't refresh

                     "--category" "app-emacs"            ;; without this constraint inquisitio
                                                         ;; is slow to moribund.
                     (deploy-query-inquisitio-search package) ;; construct search arguments
                     ))
        ;; need an error path here.
        )

      (setq show-trailing-whitespace nil)   ;; disable trailing whitespace

      ;; when we kill the buffer get rid of the window associated so the user
      ;; doesn't have to tediously clean-up.
      (add-hook 'kill-buffer-hook 'rid-window t t)
      )

    (pop-to-buffer search-buffer)
    ))

(defun elisp-in-path ( path path-type )
  "return a list of elisp files in the path"

  (filter-ls path path-type
    (type ?-)
    (path "\\.el$")))

;;----------------------------------------------------------------------
;; local function library.
;;----------------------------------------------------------------------

(defun mapc-read-buffer ( fn buffer )
  (save-excursion

    (with-current-buffer buffer
      (goto-char (point-min))

      (condition-case nil
        (progn
          (while
            (funcall fn (read (current-buffer))))
          nil)
        (error nil)) )))

(defun file-appender ( buffer )
  (lexical-let
    ((target-buffer buffer))

    (lambda ( source-file )
      (with-current-buffer target-buffer
        (goto-char (point-max))
        (insert-file-contents-literally (locate-library source-file))
        (goto-char (point-max))
        (insert (format "\n")) )) ))

(defun combine-library ( source-file )
  "combine-library"
  (interactive
    (list
      (completing-read (format "Library name (default %s): " (file-name-nondirectory buffer-file-name))
        'locate-file-completion load-path nil nil nil buffer-file-name)))

  (lexical-let*
    ((input-buffer    (generate-new-buffer "*combine*"))
     (export-buffer   (generate-new-buffer (format " Export %s" source-file)))
     (appender        (file-appender export-buffer)))

    (with-current-buffer input-buffer
      (insert-file-contents-literally (locate-library source-file)))

      (mapc-read-buffer
        (lambda (x)
          (when (eq 'require (car x))
            (lexical-let
              ((library-name (symbol-name (cadr (cadr x)))))

              (message "combine-library: adding library %s\n" library-name)
              (funcall appender library-name))) )
        input-buffer)

    (kill-buffer input-buffer)
    (pop-to-buffer export-buffer) ))


