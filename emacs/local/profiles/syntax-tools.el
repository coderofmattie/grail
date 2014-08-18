;;----------------------------------------------------------------------
;; code editing tools
;;----------------------------------------------------------------------
(require 'mode-tools)

(grail-load-package 'dash "git" "https://github.com/magnars/dash.el.git")

(grail-load-package 'smartparens "git" "https://github.com/Fuco1/smartparens.git")

(strip-minor-mode-keymap 'smartparens-mode)

(set-face-background 'sp-pair-overlay-face "grey10")
(set-face-background 'sp-wrap-overlay-face "grey10")
(set-face-background 'sp-wrap-tag-overlay-face "grey10")

(defun profile/syntax-tools-setup ()
  (interactive)
  (smartparens-mode 1))

(provide 'profile/syntax-tools)
