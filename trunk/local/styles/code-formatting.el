;;----------------------------------------------------------------------
;; code-formatting.el
;;----------------------------------------------------------------------

;; adaptive fill for maintaining indentation inside comments

(grail-activate-with-recovery "code-formatting" filladapt
  (("filladapt" . "http://www.wonderworks.com/download/filladapt.el"))

  (add-hook 'c-mode-common-hook 'c-setup-filladapt))

;; I would like to find a mode that lines up columns within code.

