;;----------------------------------------------------------------------
;; web.el - web development
;;----------------------------------------------------------------------
(require 'mode-tools)
(require 'programming-generic)
(require 'dwim-tab)

(grail-load-package 'web-mode "git" "https://github.com/fxbois/web-mode.git")

(remap-assoc-mode-to 'html-mode 'web-mode)
(remap-assoc-mode-to 'javascript-mode 'web-mode)
(remap-assoc-mode-to 'css-mode 'web-mode)

(defvar web-profile-indent-offset 2)

(setq
  web-mode-enable-current-element-highlight t

  setq web-mode-markup-indent-offset web-profile-indent-offset
  setq web-mode-css-indent-offset web-profile-indent-offset
  setq web-mode-code-indent-offset web-profile-indent-offset)

(defun web-mode-profile-setup ()
  (configure-for-select 'web-mode-element-content-select 'web-mode-element-select)

  (programming-mode-generic)

  ;; has it's own commenting
  (local-set-key (kbd "C-c ;") 'web-mode-comment-or-uncomment)

  (turn-on-dwim-tab 'web-mode-indent-line))

(add-hook 'web-mode-hook 'web-mode-profile-setup t)

(provide 'profile/web)

