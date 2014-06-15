;;----------------------------------------------------------------------
;; custom indent
;;----------------------------------------------------------------------

;; this fixes fucked up indents
(defun hard-electric-newline ()
  (interactive)

  (indent-according-to-mode)
  (newline-and-indent))

(provide 'generic-indent)





