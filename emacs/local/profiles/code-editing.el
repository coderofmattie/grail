;;----------------------------------------------------------------------
;; code editing tools
;;----------------------------------------------------------------------

(grail-load 'smartparens   (grail-define-installer "smartparens"
                            "pkg"
                            'smartparens))

(strip-minor-mode-keymap 'smartparens-mode)
(set-face-background 'sp-pair-overlay-face "grey10")
(set-face-background 'sp-wrap-overlay-face "grey10")
(set-face-background 'sp-wrap-tag-overlay-face "grey10")





