;;----------------------------------------------------------------------
;; broken.el
;; various peices of emacs that are currently broken, but worth keeping
;; and fixing.
;;----------------------------------------------------------------------


;;----------------------------------------------------------------------
;;          else (L)anguage (S)ensitive (E)diting.
;;----------------------------------------------------------------------
(require 'else-mode)

(defun tune-else-language ( lang )
  (let ((resume (current-buffer))
	 (template-file (concat (getenv "HOME") "/projects/else-mode/" lang ".lse"))
	 (template-buf ))

    (set-buffer (find-file-read-only template-file))

    (beginning-of-buffer)
    (else-compile-buffer)

    (kill-buffer template-buf)
    (set-buffer resume)
    )

  ;; localize the current language to the buffer and set it properly
  (set (make-local-variable 'else-Current-Language) lang)
  (else-mode)

  ;; C-C / is my magic prefix
  (local-set-key (kbd "<right>") 'else-next-placeholder)
  (local-set-key (kbd "<left>") 'else-prev-placeholder)

  (local-set-key "\C-c/e" 'else-expand-placeholder)
  (local-set-key "\C-c/d" 'else-kill-placeholder)
  )
