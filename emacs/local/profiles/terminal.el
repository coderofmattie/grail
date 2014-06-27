;;----------------------------------------------------------------------
;; enhanced terminal
;;----------------------------------------------------------------------
(require 'term)
(require 'buffer-ring)

;;----------------------------------------------------------------------
;;                 IPC shell:  comint/term mode
;;----------------------------------------------------------------------

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

;; term only seems to run one mode.

;; (defun terminal-profile-setup ()
;;   (configure-for-buffer-ring "prompt") )

;; (add-hook 'term-mode-hook 'terminal-profile-setup)

(setq terminal-profile-local-shell "/usr/bin/zsh")

(defun local-term ( &optional command )
  "run a terminal in a split window"
  (interactive)

  (split-window-horizontally)
  (other-window 1)

  (term
    (if command
      command
      terminal-profile-local-shell)) )

(global-set-key (kbd "C-c r t")  'split-term)

