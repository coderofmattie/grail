;;----------------------------------------------------------------------
;; code highlighting tools
;;----------------------------------------------------------------------

;; mic-paren fancy paren/delimited highlighting. It is particularly
;;           valuable for reverse highlighting regions.

(grail-load 'mic-paren (grail-define-installer "mic-paren"
                         "file"
                         "http://www.emacswiki.org/cgi-bin/emacs/download/mic-paren.el"))

;;----------------------------------------------------------------------
;; paren mode configuration - most important mode of them all
;;----------------------------------------------------------------------
(setq
  paren-showing t
  show-paren-style 'parenthesis
  show-paren-delay 1
  paren-sexp-mode 'match)

(grail-set-faces
  (paren-face-match (background "grey20")))

(paren-activate)
