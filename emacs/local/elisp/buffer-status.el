;;----------------------------------------------------------------------
;; accumulate buffer status
;;
;; when multiple status messages are needed accumulate the status messes
;; and display the list at the end.
;;----------------------------------------------------------------------
(defvar-local buffer-status-list nil)

(defun buffer-display-status ()
  (interactive)
  (let
    ((status-display-string ""))

    (when buffer-status-list
      (mapc (lambda ( status-msg )
              (setq status-display-string (if (string-equal "" status-display-string)
                                            status-msg
                                            (concat status-display-string " , " status-msg)) ))
        buffer-status-list)

      (message "%s" status-display-string)
      (setq buffer-status-list nil)) ))

(defun buffer-status-add ( message )
  (setq buffer-status-list (cons message buffer-status-list)))

(provide 'buffer-status)
