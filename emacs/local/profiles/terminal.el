;;----------------------------------------------------------------------
;; enhanced terminal
;;----------------------------------------------------------------------
(require 'term)
(require 'buffer-ring)

;;----------------------------------------------------------------------
;;                 IPC shell:  comint/term mode
;;----------------------------------------------------------------------

(defun close-term ( term-buffer term-window )
  (with-current-buffer term-buffer
    (other-window 1)

    (delete-other-windows term-window)
    (kill-buffer term-buffer) ))

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

(defun terminal-profile-setup ()
  (configure-for-buffer-ring "prompt") )

(add-hook 'term-mode-hook 'terminal-profile-setup)

(setq explicit-shell-file-name "bash")
(setq terminal-profile-local-shell "zsh")

(defun full-term ( prefix &optional command )
  "run a full terminal in full window C-u or (default) split window"
  (interactive "P")

  (unless (and prefix (equal (car prefix) 4))
    (split-window-horizontally)
    (other-window 1))

  (term
    (if command
      command
      terminal-profile-local-shell)) )

(defun shell-term ( prefix &optional command )
  "run a local shell in full window C-u or (default) split window"
  (interactive "P")

  (unless (and prefix (equal (car prefix) 4))
    (split-window-horizontally)
    (other-window 1))

  (ansi-term
    (if command
      command
      terminal-profile-local-shell)) )

(global-set-key (kbd "C-c x t") 'local-term)
(global-set-key (kbd "C-c x s") 'local-term)
(global-set-key (kbd "C-c x c") 'shell-command) ;; works on remote host with tramp

