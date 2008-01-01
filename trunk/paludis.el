(require 'auto-overlays)
(require 'auto-overlay-word)

(defun paludis-test ()
  (interactive)
;;  (highlight-regexp "^\\\*[[:blank:]]+")
  (highlight-regexp "^\\\*[[:blank:]]+\\([^[:blank:]]+\\)[[:blank:]]*$")
  )

(defun paludis-mode ()
  "turn on paludis mode"
  (interactive)

  (auto-overlay-unload-regexp 'paludis)
  (auto-overlay-load-regexp
    `(word ("^\\\*[[:blank:]]+\\([^[:blank:]]+\\)[[:blank:]]*$" . 1)
       (face . (underline . t))
       )
    'paludis
    )

  (auto-overlay-start 'paludis)
  )

(provide 'paludis)
