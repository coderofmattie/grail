;;----------------------------------------------------------------------
;; enhanced terminal
;;----------------------------------------------------------------------

(require 'term)

;; terminal stuff

(setq explicit-shell-file-name "/bin/zsh")

(defun split-term ( &optional command )
  "run a terminal in a split window"
  (interactive)

  (split-window-horizontally)
  (other-window 1)

  (term
    (if command
      command
      explicit-shell-file-name)) )

(defun close-term ( term-buffer term-window )
  (other-window 1)
  (delete-other-windows term-window)
  (kill-buffer term-buffer) )

(add-hook 'term-exec-hook
  (lambda ()
    (let*
      ((buff (current-buffer))
       (win  (get-buffer-window))
       (proc (get-buffer-process buff)))

      (lexical-let
        ((term-buffer buff)
         (term-window win))

        (set-process-sentinel proc
          (lambda (process event)
            (if (string= event "finished\n")
              (close-term term-buffer term-window)) )) ) )) )

;; (grail-load 'multi-term (grail-define-installer "multi-term"
;;                          "file"
;;                          "http://www.emacswiki.org/emacs/download/multi-term.el"))

;; (setq multi-term-program "/bin/zsh")

;; (defun sys ()
;;   (interactive)
;;   (multi-term))

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
