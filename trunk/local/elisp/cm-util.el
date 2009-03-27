;;----------------------------------------------------------------------
;; cm-util.el
;;----------------------------------------------------------------------

(defun split-window-to-buffers ( large-window-buffer small-window-buffer )
  "split-window-to-buffers LARGE SMALL

   Delete any other windows, then split the window: one large one
   small. Display the buffers LARGE SMALL in the respective
   buffers.
  "
  (delete-other-windows)
  (let
    ((size (/ (window-height) 3))
     (old-window (selected-window))
     (new-window nil))

    ;; this x = (size / 3) + x / 3 produces a nicer looking split I think.
    (setq size (+ size (/ size 3)))

    (setq new-window (split-window old-window size))

    ;; don't record the switch
    (select-window old-window t)
    (switch-to-buffer (or (when (functionp small-window-buffer) (funcall small-window-buffer)) small-window-buffer))
    (select-window new-window t)
    (switch-to-buffer (or (when (functionp large-window-buffer) (funcall large-window-buffer)) large-window-buffer)) ))

(defun buffer-empty-p ( &optional buffer )
  "buffer-empty-p &optional buffer

   buffer-empty-p return t if the buffer is empty"
  (with-current-buffer (or (when (bufferp buffer) buffer)
                           (current-buffer))
    (not (> (point-max) (point-min)))) )

;; required for my patched em-unix, note: merged upstream, may collide
;; on a update.
(defun nil-blank-string ( string )
  "if a string is all blanks return nil, if there are non-blank characters return the string"
  (if (string-match "[^[:blank:]]" string ) string))

(provide 'cm-util)

