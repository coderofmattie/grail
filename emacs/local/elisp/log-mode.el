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
  '(log-mode-dwim-init)
    "major mode for editing log files")

(defun log-mode-dwim-init ()
  (turn-on-dwim-tab 'cperl-indent-command))

(provide 'log-mode)
