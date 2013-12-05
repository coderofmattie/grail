;;----------------------------------------------------------------------
;; log-mode.el
;;----------------------------------------------------------------------

(define-generic-mode 'log-mode
  '("#")
  '("summary:" "description:")
  '(("summary:" . 'font-lock-keyword-face)
    ("description:" . 'font-lock-keyword-face)
    ("\\[.*\\]". 'font-lock-function-name-face))
    '("\\.log$")
    nil
    "major mode for editing log files")

(provide 'log-mode)
