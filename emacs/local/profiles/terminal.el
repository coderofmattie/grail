;;----------------------------------------------------------------------
;; enhanced terminal
;;----------------------------------------------------------------------
(require 'term)
(require 'buffer-ring)
(require 'custom-key)

;;----------------------------------------------------------------------
;;                 IPC shell:  comint/term mode
;;----------------------------------------------------------------------

(defadvice term (after terminal/after-hook)
  (proc-close-on-exit/window))

(ad-activate 'term)

(setq explicit-shell-file-name "bash")
(setq terminal-profile-local-shell "zsh")

(defadvice term (after terminal-profile/term-bindings)
  (configure-for-buffer-ring "prompt"))

(defadvice ansi-term (after terminal-profile/ansi-bindings)
  (configure-for-buffer-ring "prompt"))

(ad-activate 'term)
(ad-activate 'ansi-term)

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

(custom-key-group "execute" "x" t
    ("t" . full-term)
    ("s" . shell-term)
    ("c" . shell-command) )

