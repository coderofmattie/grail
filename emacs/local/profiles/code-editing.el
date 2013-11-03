;;----------------------------------------------------------------------
;; code editing tools
;;----------------------------------------------------------------------

(grail-load 'smartparens   (grail-define-installer "smartparens"
                            "pkg"
                            'smartparens))

(strip-minor-mode-keymap 'smartparens-mode)






