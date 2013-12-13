;;----------------------------------------------------------------------
;; ext-logging.el - support for external logging
;;
;; description:
;;
;; viewing log files effectively is a good target for emacs. implement
;; log viewing hooks and modes.
;;----------------------------------------------------------------------

(defun load-logging-file (log-file)
  (switch-to-buffer (find-file-noselect log-file))
  (rename-buffer (concat "logwatch: " log-file))

  (auto-revert-tail-mode)
  (setq buffer-read-only t)
  (goto-char (point-max)) )

(provide 'ext-logging)
