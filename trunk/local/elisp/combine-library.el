;;----------------------------------------------------------------------
;; local function library.
;;----------------------------------------------------------------------

(defun mapc-read-buffer ( fn buffer )
  "mapc-read-buffer FN BUFFER

   read through BUFFER returning a sequence reads as sexp's passed to FN
   with the signature of (FN sexp begin end).

   The process continues until the buffer is exhausted or FN returns nil.
  "
  (save-excursion
    (with-current-buffer buffer
      (goto-char (point-min))

      (condition-case nil
        (progn
          (while
            (lexical-let*
              ;; this really doesn't work because the reader simply skips over
              ;; any comments/whitespace and returns the instructions.

              ;; a better way would be to scan for the require lines, and do
              ;; a replace on them. Use thing-at-point or something like it
              ;; to find the bounds and replace with a insert.
              ((begin (point))
               (sexp  (read (current-buffer)))
               (end   (point)))
               (and sexp (funcall fn sexp begin end))))
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
        (insert (format "\n")) )
      t) ))

(defun combine-library ( source-file )
  "combine-library source-file

   An interactive command to merge the dependencies for a module. This assumes that
   the dependencies have been organized into a file listing all the dependencies.

   For example if there is a module foo that depends on bar and baz the files would
   look like this:

   foo.el:

   .....
   (require 'foo-fn)
   .....

   foo-fn.el:

   .....
   (require 'bar)
   (require 'baz)

   (provide 'foo-fn)
   .....
  "
  (interactive
    ;; prompt for the file name with completion.
    (list
      (completing-read
        (format "Library name (default %s): "
          ;; BUG: file-names are only present on buffers with a backing,
          ;; for eshell buffers etc it explodes. Need to simply go to the
          ;; cwd when all else fails.
          (file-name-nondirectory buffer-file-name))
        'locate-file-completion load-path nil nil nil buffer-file-name) ))

  (lexical-let*
    ((input-buffer    (generate-new-buffer "*combine*"))
     (export-buffer   (generate-new-buffer (format " Export %s" source-file)))
     (appender        (file-appender export-buffer)))

    (with-current-buffer input-buffer
      (insert-file-contents-literally (locate-library source-file)))

      (mapc-read-buffer
        (lambda (sexp begin end)
          (if (eq 'require (car sexp))
            (lexical-let
              ((library-name (symbol-name (cadr (cadr sexp)))))

              (message "combine-library: adding library %s\n" library-name)
              (funcall appender library-name))
            (append-to-buffer export-buffer begin end) ))
        input-buffer)

    (kill-buffer input-buffer)
    (pop-to-buffer export-buffer) ))

(provide 'combine-library)
