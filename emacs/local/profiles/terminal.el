;;----------------------------------------------------------------------
;; enhanced terminal
;;----------------------------------------------------------------------
(grail-load 'multi-term (grail-define-installer "multi-term"
                         "pkg"
                         'multi-term))

(setq multi-term-program "/bin/zsh")

(defun sys ()
  (interactive)
  (multi-term))

;; old code that may be useful.

;; ;; setup a hook that runs when the term process exits
;; (defvar term-process-exit-hook nil
;;   "a hook run when the term process exists")

;; ;; infect the term-sentinel function
;; (defadvice term-sentinel (around term-sentinel-hook (proc msg))
;;   (let
;;     ((pbuffer (process-buffer proc))
;;      (did-exit (memq (process-status proc) '(signal exit))))
;;     ad-do-it
;;     (when did-exit (run-hooks-with-arg term-process-exit-hook pbuffer)) ))

;; ;; get rid of the buffer when the process exits
;; (add-hook 'term-process-exit-hook (lambda ( pbuffer )
;;                                     (kill-buffer pbuffer))
;;   t)

;; (ad-activate 'term-sentinel)
