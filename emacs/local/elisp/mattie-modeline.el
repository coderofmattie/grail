;;----------------------------------------------------------------------
;; mattie-modeline
;;----------------------------------------------------------------------
(defun mattie-modeline-branch-null ()
  "")

(defvar mattie-modeline-branch 'mattie-modeline-branch-null)

(defun mattie-modeline-modified ()
  "mattie-modeline-modified

   Construct a modified string for the modeline.
  "
  (concat
    "[" (symbol-name buffer-file-coding-system) "]"
    " "

    "["
    (if (or (and (local-variable-p 'view-read-only) view-read-only)
          (and (local-variable-p 'buffer-read-only) buffer-read-only))
      "RO"
      "RW")
    "/"
    (cond
      ((not (verify-visited-file-modtime (current-buffer))) "!DSK")
      ((buffer-modified-p)  "!CHG")
      ((and buffer-file-name (recent-auto-save-p)) "!BKP")
      (t "-"))
    "/"
    (if (and (boundp 'mattie-modeline-vcs) (functionp mattie-modeline-vcs))
      (funcall mattie-modeline-vcs)
      "")
    (if wc-modeline-status wc-modeline-status "")
    "]"))

(defun setup-mattie-modeline ()
  (setq-default mode-line-format
    '(" "
      "[" (:eval (buffer-name)) "]"

      " "

      (line-number-mode "%l")
      ">"
      (column-number-mode "%c")

      " "

      (:eval (mattie-modeline-modified))

      " "

      "[" (:propertize mode-name face (:weight bold)) "] "

      buffer-ring-modeline

      wc-modeline))
  (force-mode-line-update))

(provide 'mattie-modeline)
