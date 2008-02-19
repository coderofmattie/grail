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
  "create the merge copy of the file"
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
  (interactive "fFind source Working Copy? ")
  (load-or-copy-ancestor
    'working-copy-path
    'working-copy-name
    'insert-merge))

