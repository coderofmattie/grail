;;----------------------------------------------------------------------
;; merc.el
;; Primary Author: Mike Mattie
;;----------------------------------------------------------------------

;; Modern Emacs Revision Consolidation

;; Use quilt to stage changes to a public repository supporting
;; cherry picking of changes into queues for committing.

;;----------------------------------------------------------------------
;; utilities
;;----------------------------------------------------------------------
(defun buffer-empty-p ()
  (not (> (point-max) (point-min))))

;;----------------------------------------------------------------------
;; buffer creation
;;----------------------------------------------------------------------

(define-error merc-error "An error signal generated within merc.el")
(define-error merc-unkown "Buffer was not created by merc so merc commands do not apply" merc-error)

(defun merc-set-auto-mode ( file )
  "run set-auto-mode with the original file-name visible so that the
   file extension based matching will work correctly"
  (let
    ((buffer-file-name file)) ;; set-auto-mode uses filename. shadow it.
    (set-auto-mode t)))

(defun merc-insert-checkout ( buffer file )
  (let
    ((default-directory (file-name-directory file)))
    (= 0 (call-process "svn" nil t t "cat" (file-name-nondirectory file)))))

(defun load-or-copy-ancestor ( file extension type &optional fetch-copy )
  "load or copy the ancestor of FILE.

   A buffer is either found or created. If it is an empty file
   the ancestor of the file is inserted into the buffer.

   Checkout -> Merge Copy -> Working Copy

   The major mode is set using the original file-name so that the
   extensions to differentiate the copies on disk do not baffle
   set-auto-mode. The buffer is renamed to indicate that all the
   buffer preparation work has been completed.
  "
  (lexical-let*
    ((path    (expand-file-name
                (if extension
                  (concat file "." extension)
                  file)))
     (buffer  (or
                (find-buffer-visiting path)
                (find-file path)))

     (name    (concat (file-name-nondirectory file) "/" type)))

  (with-current-buffer buffer
    (unless (boundp 'merc-target)
      (make-local-variable 'merc-target)
      (setq merc-target file))

    (when (buffer-empty-p)
      (when (functionp fetch-copy)
        (funcall fetch-copy buffer file)
        (write-file path)))

    (unless (string-equal (buffer-name) name)
      (merc-set-auto-mode file)
      (rename-buffer name)) )

    buffer))

(defun merc-target ()
  (if (boundp 'merc-target)
    merc-target
    (signal 'merc-unkown this-command)))

(defun mc-exists-p ( file )
  (file-readable-p (merc-mc-path file)))

(defun merc-mc ( file )
  ;; the merge copy could be turned into a interface into quilt.
  (load-or-copy-ancestor
    file
    "merge-copy"
    "merge"
    'merc-insert-checkout))

(defun mc ( file )
  "Find the Merge Copy of a source file in a existing buffer, by loading the file from disk,
   or finally by checking out the latest published version.

   The Merge Copy should not be directly modified normally, it's managed by quilt."
  (interactive "fFind source Merge Copy? ")
  (switch-to-buffer (merc-mc file)))

(defun merc-insert-merge ( buffer file )
  (with-current-buffer (mc file)
    (copy-to-buffer buffer (point-min) (point-max))))

(defun wc-exists-p ( file )
  (file-readable-p (merc-wc-path file)))

(defun merc-wc ( file )
  (load-or-copy-ancestor
    file
    "working-copy"
    "wc"
    'merc-insert-merge))

(defun wc ( file )
  "Find the Working Copy of a source file in a existing buffer, by loading the file from disk,
   or finally by copying the Merge Copy.

   The Working Copy is where new changes are made directly."
  (interactive "fFind source Working Copy? ")
  (switch-to-buffer (merc-wc file)))

(defun merc-cc ( file )
  (load-or-copy-ancestor
    file
    nil
    "checkout"))

(defun cc ( file )
  "Find the Checkout copy of a source file"
  (interactive "fFind source Checkout Copy? ")
  (switch-to-buffer (merc-cc file)))

(defun merc-diff ()
  "merc diff is a simple form of cherry picking that uses ediff"
  (interactive)
  (save-excursion
    (ediff-buffers (merc-cc (merc-target)) (current-buffer)) ))

;;----------------------------------------------------------------------
;; merge-queue
;;----------------------------------------------------------------------

;; locate or establish the merge-queue for this checkout. The FS location
;; of the merge-queue is cached in a buffer local variable.

;; Use the accessor get-merge-queue to return the path of the queue.

(defun ascend-to-checkout-root ( dir )
  "The recursive core of find-checkout-root. use that entry point instead."
  (if (and
        (file-accessible-directory-p dir)
        (file-writable-p (concat dir "/_")))

    ;; vc-backend is not robust with inputs. If a directory is given without a trailing
    ;; slash a nil value will be returned incorrectly for directories under version
    ;; control.

    (if (vc-backend dir)
      (lexical-let
        ((traverse (strip-list-last (split-string dir "/" t))))

        (if traverse
          (lexical-let
            ((found (ascend-to-checkout-root (prefix-strings "/" traverse))))
            (if (eq 't found)
              dir
              found))
          t))   ;; halt when list is exhausted
      t)        ;; halt when the directory is no longer under version control.
    t))         ;; halt when we don't have read and write permission for the directory.

(defun find-checkout-root ( dir )
  "Find the ancestor directory that is the root of the checkout containing DIR.

   Recursion halts on these conditions:
   * exhausted path.
   * directory is not: accesible,readable, and writable.
   * directory is not under version control according to vc-backend.
  "
  (lexical-let
    ((found (ascend-to-checkout-root dir)))
    (if (eq 't found)
      dir
      found)))

(defun find-merge-queue ( dir )
  (find-child-directory-in-ancestor merge-queue-directory-name dir))

(defvar merge-queue-directory-name "quilt-queue"
  "the name of the quilt directory")

(defun find-or-create-merge-queue ( dir )
  "Find or create the merge-queue directory for this working copy checkout.

   The search for a existing merge-queue directory will continue above the
   root of a checkout. If an existing queue is not found the queue will
   be created at the root of the checkout."
  (lexical-let
    ((queue (find-merge-queue dir)))

    (unless queue
      (setq queue (concat (find-checkout-root dir) "/" merge-queue-directory-name))
      (make-directory queue t)) ;; TODO: signal an IO error here, caught by all command entry points.

    queue))

(defun get-merge-queue ()
  (unless (boundp 'merc-target) (signal 'merc-unkown this-command))

  (unless (boundp 'merc-queue)
    (make-local-variable 'merc-queue)
    (setq merc-queue (find-or-create-merge-queue (file-name-directory (buffer-file-name)))))

  merc-queue)

(defun merge-queue-merge-plan ()
  (find-file-literally (concat (get-merge-queue) "/merge-plan")))

(defun append-to-merge-plan ( commit-dir )
  (with-current-buffer (merge-queue-merge-plan)
    (goto-char (point-max))
    (insert (format "%s\n" commit-dir))
    (basic-save-buffer)))

;; completing read

(defun new-commit ( name )
  "Create a new commit in the Merge Queue. The commit is automatically
   appended to the Merge Plan."
  (interactive "Mcommit name? ")
  (if (< (length name) 1)
    (message "aborted new-commit.")
    (save-excursion
      (make-directory (concat (get-merge-queue) "/" name) t)
      (append-to-merge-plan name)) ))
