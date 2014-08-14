;;----------------------------------------------------------------------
;; code editing tools
;;----------------------------------------------------------------------
(require 'mode-tools)

(grail-load 'dash (grail-define-installer "dash"
                   "git"
                   "https://github.com/magnars/dash.el.git"))

(grail-load 'smartparens (grail-define-installer "smartparens"
                          "git"
                          "https://github.com/Fuco1/smartparens.git"))

(strip-minor-mode-keymap 'smartparens-mode)
(set-face-background 'sp-pair-overlay-face "grey10")
(set-face-background 'sp-wrap-overlay-face "grey10")
(set-face-background 'sp-wrap-tag-overlay-face "grey10")

(defun procedural-smart-parens-editing ()
  (interactive)
  (smartparens-mode 1))

(provide 'grail/code-editing)
