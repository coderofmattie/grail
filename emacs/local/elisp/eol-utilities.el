;;----------------------------------------------------------------------
;; eol utilities.
;;----------------------------------------------------------------------
(require 'buffer-status)

(defun warn-if-dos-eol-in-buffer ()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (when (search-forward-regexp "$" nil t)
      (buffer-status-add "!WARNING! DOS EOL lines"))))

(defun scrub-dos-eol ()
  (interactive)
  (let
    ((bad-eol-regex "$")
     (good-eol-character ""))

    (save-excursion
      (let
        ((end-point (progn (end-of-buffer) (point))))

        (beginning-of-buffer)

        (replace-regexp bad-eol-regex good-eol-character nil (point) end-point) )) )

  (when (> (length buffer-file-name) 0)
    (save-buffer)) )

(add-hook 'after-change-major-mode-hook 'warn-if-dos-eol-in-buffer t)

(provide 'eol-utilities)
