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
  (concat (file-name-nondirectory file) "/WC"))

(defun merge-copy-name ( file )
  (concat (file-name-nondirectory file) "/Merge"))

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

