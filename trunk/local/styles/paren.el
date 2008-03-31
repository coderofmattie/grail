;;----------------------------------------------------------------------
;; paren.el
;;----------------------------------------------------------------------
(require 'mic-paren)

;; fancy paren/delimited highlighting.

(paren-activate)

;; eldoc can be a useful minor mode. It displays the function signature
;; documentation on a idle timer. I find it to be too distracting for
;; little gain over c-h-f.

(setq
  paren-showing t
  show-paren-style 'parenthesis
  show-paren-delay 2
  paren-sexp-mode 'match )


