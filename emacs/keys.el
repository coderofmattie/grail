;;----------------------------------------------------------------------
;; keys.el
;;
;; keybinding tools and configuration.
;;----------------------------------------------------------------------
(require 'custom-key)

;; remove keybindings
(global-unset-key (kbd "<S-tab>"))

(custom-key-group "window manager" "w" t
  ("k" . delete-frame)
  ("m" . delete-other-windows)
  ("h" . split-window-horizontally)
  ("v" . split-window-vertically) )

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

;; line number mode
(global-set-key (kbd "C-c C-l")  'linum-mode)

(custom-key-group "files" "f" t
    ("d" . dired)
    ("s" . save-some-buffers))

