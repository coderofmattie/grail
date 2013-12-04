;;----------------------------------------------------------------------
;; web.el - web development
;;----------------------------------------------------------------------
(require 'remap-assoc-mode)

(grail-load 'web-mode (grail-define-installer "web-mode"
                          "git"
                          "https://github.com/fxbois/web-mode.git"))

(remap-assoc-mode-to 'html-mode 'web-mode)
(remap-assoc-mode-to 'javascript-mode 'web-mode)
(remap-assoc-mode-to 'css-mode 'web-mode)

(defvar web-profile-indent-offset 2)

(setq
  web-mode-enable-current-element-highlight t

  setq web-mode-markup-indent-offset web-profile-indent-offset
  setq web-mode-css-indent-offset web-profile-indent-offset
  setq web-mode-code-indent-offset web-profile-indent-offset)

(defun web-mode-hook-fn ()
  (configure-for-select 'web-mode-element-content-select 'web-mode-element-select)

  (configure-for-programming nil "web-code")
  (configure-for-navigation 'web-mode-element-traverse 'web-mode-element-previous)

  ;; has it's own commenting
  (local-set-key (kbd "C-c ;") 'web-mode-comment-or-uncomment)

  (turn-on-dwim-tab 'web-mode-indent-line)

  ;; for some bizarre reason it only fontifies if font-lock is off.
  (font-lock-mode 0))

(add-hook 'web-mode-hook 'web-mode-hook-fn t)
