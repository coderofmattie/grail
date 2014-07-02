;;----------------------------------------------------------------------
;; keys.el
;;
;; keybinding tools and configuration.
;;----------------------------------------------------------------------

;; remove keybindings
(global-unset-key (kbd "<S-tab>"))

(defun wm-bindings ()
  (let
    ((ver-map (make-sparse-keymap)))

    (define-key ver-map "k" 'delete-frame)
    (define-key ver-map "m" 'delete-other-frames)

    (define-key ver-map "h" (keybindings-help-fn "window manager" ver-map))
    (local-set-key (kbd "C-c w") ver-map)))

(wm-bindings)

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

