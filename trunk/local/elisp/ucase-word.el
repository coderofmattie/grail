;;;----------------------------------------------------------------------
;; ucase-word.el
;; written by Mike Mattie
;;;----------------------------------------------------------------------

(defun toggle-ucase-word ()
  "ucase-word

   When this command is executed text insertions will be capitalized
   until a trailing space is encountered in the text.

   Essentially it turns on a soft cap-lock until you type space.

   If ucase-word is already enabled it will disable it.
  "
  (interactive)
  (if (memq 'ucase-word-hook after-change-functions)
    (progn
      (message "turning off ucase-word")
      (remove-hook 'after-change-functions 'ucase-word-hook t))
    (progn
      (message "text will be capitalized until a space ends the insert")
      (add-hook 'after-change-functions 'ucase-word-hook t t))))

(defun ucase-word-hook ( begin end before-len )
  (upcase-region begin end)
  (if (looking-back "[ \n]" 1)
    (remove-hook 'after-change-functions 'ucase-word-hook t)))

(provide 'ucase-word)
