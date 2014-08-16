;;----------------------------------------------------------------------
;; commands.el
;;----------------------------------------------------------------------

(require 'ucase-word)
(require 'buffer-status)
(require 'cl)

(require 'whitespace-utilities)
(require 'eol-utilities)

(defun scrub-all ()
  (interactive)
  (if (buffer-modifiable-p)
    (progn
      (scrub-dos-eol)
      (scrub-tabs) )
    (message "cannot scrub in a read-only buffer!") ))

(defun print-hex ( number )
  "print the hex of a number, faster than firing up calc mode"
  (message "the hex is %x" number))

(defun rid-window ()
  "get rid of the current window"
  (interactive)
  (delete-windows-on (current-buffer)))

(defun insert-key-notation ()
  "inject a complete \(kbd \"sequence\"\) with key notation for a key sequence given by prompt"
  (interactive)
  (insert "(kbd \"")
  (insert (format-kbd-macro (read-key-sequence "Key? " nil t)))
  (insert "\")"))

(defun visit-url ( url )
  "visit a url in a new buffer"

  ;; it would be cooler if the default was the last item from the clipboard.
  (interactive "sURL? ")
  (progn
    (switch-to-buffer (generate-new-buffer url))
    (url-insert-file-contents url)))

(defun export-buffer-to-clipboard ()
  "copy the entire buffer to the clipboard"
  (interactive)
  (mark-whole-buffer)
  (copy-region-to-clipboard))

(defun show-call ( fn )
  "show-call

   Call a function printing the return value of the function as a message.
   This is really handy for seeing what a function does in the current
   buffer.
  "
  (interactive "afunction? ")
  (message "returned: %s" (princ (funcall fn))) )

(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(defun copy-buffer ( &rest args )
  (interactive)

  (save-excursion
    (let
      ((end   (progn
                (end-of-buffer)
                (point)))
        (start (progn
                 (beginning-of-buffer)
                 (point))) )

      (copy-region-as-kill start end) )))

(defun copy-region-to-clipboard ()
  "copy the region to the clipboard"
  (interactive)
  (x-select-text (filter-buffer-substring (region-beginning) (region-end))) )

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

(defun fullscreen-frame ()
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
    '(2 "_NET_WM_STATE_FULLSCREEN" 0)))

(provide 'user-commands)
