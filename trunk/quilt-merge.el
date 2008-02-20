;;----------------------------------------------------------------------
;; quilt-merge.el
;; Primary Author: Mike Mattie
;;----------------------------------------------------------------------

;; Use quilt to stage changes to a public repository supporting
;; cherry picking of changes into queues for committing.

(defun working-copy-path ( file )
  (concat file ".working-copy"))

(defun merge-copy-path ( file )
  (concat file ".merge"))

(defun working-copy-name ( file )
  (concat (file-name-nondirectory file) "/wc"))

(defun merge-copy-name ( file )
  (concat (file-name-nondirectory file) "/merge"))

(defun set-auto-mode-by-path ( file )
  (let
    ((buffer-file-name file)) ;; set-auto-mode uses filename. shadow it.
    (set-auto-mode t)))

(defun buffer-empty-p ()
  (not (> (point-max) (point-min))))

(defun insert-published ( buffer file )
  (let
    ((default-directory (file-name-directory file)))
    (= 0 (call-process "svn" nil t t "cat" (file-name-nondirectory file)))))

(defun load-or-copy-ancestor ( file make-path make-name fetch-copy )
  "load or copy the ancestor of FILE.

   A buffer is either found or created. If it is an empty file
   the ancestor of the file is inserted into the buffer.

   Published -> Merge Copy -> Working Copy

   The major mode is set using the original file-name so that the
   extensions to differentiate the copies on disk do not baffle
   set-auto-mode. The buffer is renamed to indicate that all the
   buffer preparation work has been completed.
  "
  (lexical-let*
    ((path    (funcall make-path file))

     (buffer  (or
                (find-buffer-visiting path)
                (find-file path)))
     (name    (funcall make-name file)))

  (with-current-buffer buffer
    (unless (boundp 'quilt-merge-target)
      (make-local-variable 'quilt-merge-target)
      (setq quilt-merge-target file))

    (when (buffer-empty-p)
      (funcall fetch-copy buffer file)
      (write-file path))

    (unless (string-equal (buffer-name) name)
      (set-auto-mode-by-path file)
      (rename-buffer name)) )

    buffer))

(defun mc-exists-p ( file )
  (file-readable-p (merge-copy-path file)))

(defun mc ( file )
  "Find the Merge Copy of a source file in a existing buffer, by loading the file from disk,
   or finally by checking out the latest published version.

   The Merge Copy should not be directly modified normally, it's managed by quilt."
  (interactive "fFind source Merge Copy? ")

  ;; the merge copy could be turned into a interface into quilt.

  (load-or-copy-ancestor
    (expand-file-name file)
    'merge-copy-path
    'merge-copy-name
    'insert-published))

(defun insert-merge ( buffer file )
  (with-current-buffer (mc file)
    (copy-to-buffer buffer (point-min) (point-max))))

(defun wc-exists-p ( file )
  (file-readable-p (working-copy-path file)))

(defun wc ( file )
  "Find the Working Copy of a source file in a existing buffer, by loading the file from disk,
   or finally by copying the Merge Copy.

   The Working Copy is where new changes are made directly."
  (interactive "fFind source Working Copy? ")
  (load-or-copy-ancestor
    file
    'working-copy-path
    'working-copy-name
    'insert-merge))

(defvar merge-queue-directory-name "quilt-queue"
  "the name of the quilt directory")

(defun find-merge-queue ( dir )
  (find-child-directory-in-ancestor merge-queue-directory-name dir))

(defun ascend-to-checkout-root ( dir )
  (if (and
        (file-accessible-directory-p dir)
        (file-writable-p (concat dir "/_")))

    ;; okay, vc-backend is daft in that it returns the backend of a directory only with
    ;; a trailing slash ?

    ;; oddly enough vc-backend actually returns nil at the working copy root, even though
    ;; you would think it would return a backend.
    (if (vc-backend dir)
      (lexical-let
        ((traverse (strip-list-last (split-string dir "/" t))))

        (if traverse
          (lexical-let
            ((found (ascend-to-checkout-root (concat "/" (infix-strings "/" traverse)))))
            (if (eq 't found)
              dir
              found))
          t))   ;; halt when list is exhausted
      t)        ;; halt when the directory is no longer under version control.
    t))         ;; halt when we don't have read and write permission for the directory.

(defun find-checkout-root ( dir )
  (lexical-let
    ((found (ascend-to-checkout-root dir)))
    (if (eq 't found)
      dir
      found)))

(defun find-or-create-merge-queue ( dir )
  ;; this could be turned into a single function, so that only one IO bound traverse is
  ;; performed.
  (lexical-let
    ((queue (find-merge-queue dir)))

    (unless queue
      (setq queue (concat (find-checkout-root dir) "/" merge-queue-directory-name))
      (make-directory queue t))

    queue))

(defun get-merge-queue ()
  ;; should check that quilt-merge-target is set, to see if it's a buffer initialized
  ;; by load-or-copy-ancestor
  (unless (boundp 'quilt-merge-queue)
    (make-local-variable 'quilt-merge-queue)
    (setq quilt-merge-queue (find-or-create-merge-queue (file-name-directory (buffer-file-name)))))

  quilt-merge-queue)

(defun append-to-merge-plan ( commit-dir )
  (with-current-buffer (find-file-literally (concat (get-merge-queue) "/merge-plan"))
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

