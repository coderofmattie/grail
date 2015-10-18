;;----------------------------------------------------------------------
;; keys.el
;;
;; keybinding tools and configuration.
;;----------------------------------------------------------------------
(require 'custom-key)
(require 'user-commands)
(require 'ucase-word)

;; remove keybindings
(global-unset-key (kbd "<S-tab>"))

(custom-key-group "window manager" "w" t
  ("k" . delete-frame)
  ("x" . delete-other-windows)
  ("s" . split-window-horizontally)
  ("v" . split-window-vertically)
  ("m" . maximize-frame)
  ("f" . fullscreen-frame)
  ("n" . make-frame-command) )

(global-set-key (kbd "C-x e") 'eval-expression)

;; this used to be minimize window, now it exits recursive editing
;; which is handy and safer.
(global-set-key (kbd "C-z")     'top-level)

;; reverse the regex/regular isearch

(global-set-key (kbd "C-s")     'isearch-forward-regexp)
(global-set-key (kbd "C-r")     'isearch-backward-regexp)

(global-set-key (kbd "M-r")     'query-replace-regexp)

;; standard emacs prompt for a interactive command
(global-set-key (kbd "<escape>") 'execute-extended-command)

;; other window is more useful. there is no really good way
;; for buffer switching outside of buffer ring

;; using it for complete now. will come up with something else.
;; (global-set-key (kbd "<M-tab>") 'other-window-non-interactive)

(global-set-key (kbd "<prior>") 'beginning-of-buffer)
(global-set-key (kbd "<next>")  'end-of-buffer)

(global-set-key (kbd "M-g")  'goto-line)

(global-set-key (kbd "<backtab>") 'next-buffer)

(global-set-key (kbd "<f1>") 'toggle-ucase-word)

;; line number mode
(global-set-key (kbd "C-c C-l")  'linum-mode)

(defvar user-keys/tree-browser nil)

(defun user-keys/start-tree-browser ()
  "user-keys/start-tree-browser

   start a directory tree browser if one
   has been loaded.
  "
  (interactive)

  (if user-keys/tree-browser
    (let
      (( current-prefix-arg nil ))

      (call-interactively user-keys/tree-browser)
      (funcall user-keys/tree-browser) )
    (message "user-config: no tree browser loaded") ))

(defun pop-dired-in-file ()
  "pop-dired-in-file

   pop a dired buffer in the directory of the current file.
  "
  (interactive)
  (dired-other-window (file-name-directory buffer-file-name)) )

(custom-key-group "files" "f" t
    ("d" . dired)
    ("c" . pop-dired-in-file)
    ("n" . user-keys/start-tree-browser)
    ("s" . save-some-buffers)
    ("b" . hexl-find-file))

(custom-key-group "mark" "m" t
  ("x" . exchange-point-and-mark)
  ("p" . push-mark-command)
  ("g" . pop-global-mark))




