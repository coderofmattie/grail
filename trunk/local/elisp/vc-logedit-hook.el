;;----------------------------------------------------------------------
;; vc-logedit-hook.el
;; written by Mike Mattie
;;----------------------------------------------------------------------

(defvar vc-new-logedit-hook nil
  "a hook to vc-start-logentry that is run when a new logedit
   buffer is created. It runs after the logedit buffer has
   finished setup, and the initial content if any has been inserted.")

(defun vc-start-logentry (files extra comment initial-contents msg logbuf action &optional after-hook)
  "Accept a comment for an operation on FILES with extra data EXTRA.
If COMMENT is nil, pop up a LOGBUF buffer, emit MSG, and set the
action on close to ACTION.  If COMMENT is a string and
INITIAL-CONTENTS is non-nil, then COMMENT is used as the initial
contents of the log entry buffer.  If COMMENT is a string and
INITIAL-CONTENTS is nil, do action immediately as if the user had
entered COMMENT.  If COMMENT is t, also do action immediately with an
empty comment.  Remember the file's buffer in `vc-parent-buffer'
\(current one if no file).  AFTER-HOOK specifies the local value
for `vc-log-after-operation-hook'."
  (let ((parent
         (if (vc-dispatcher-browsing)
             ;; If we are called from a directory browser, the parent buffer is
             ;; the current buffer.
             (current-buffer)
           (if (and files (equal (length files) 1))
               (get-file-buffer (car files))
             (current-buffer)))))

    (let
      ((log-buffer-already-exists (get-buffer logbuf)))

      (if (and comment (not initial-contents))
	(set-buffer (get-buffer-create logbuf))
        (pop-to-buffer (get-buffer-create logbuf)))

      (set (make-local-variable 'vc-parent-buffer) parent)
      (set (make-local-variable 'vc-parent-buffer-name)
        (concat " from " (buffer-name vc-parent-buffer)))

      (vc-log-edit files)

      (make-local-variable 'vc-log-after-operation-hook)
      (when after-hook
        (setq vc-log-after-operation-hook after-hook))
      (setq vc-log-operation action)
      (setq vc-log-extra extra)

      ;; don't run the hook when there is a comment as they are
      ;; obviously discarding

      (when comment
        (erase-buffer)
        (when (stringp comment) (insert comment)))

      (if (or (not comment) initial-contents)
        (progn
          ;; when creating a new logedit buffer run vc-before-logedit-hook
          ;; it runs only after the buffer is fully prepared.
          (when (and (not log-buffer-already-exists) vc-new-logedit-hook))
          (run-hooks 'vc-new-logedit-hook)
	(message "%s  Type C-c C-c when done" msg))

      (vc-finish-logentry (eq comment t))))))

(provide 'vc-logedit-hook)
