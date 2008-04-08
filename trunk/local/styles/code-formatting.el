;;----------------------------------------------------------------------
;; code-formatting.el
;;----------------------------------------------------------------------

(require 'filladapt)

;; adaptive fill for maintaining
;; indenting inside comments

(add-hook 'c-mode-common-hook 'c-setup-filladapt)

;; I would like to find a mode that lines up columns within code.

