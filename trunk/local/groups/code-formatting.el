;;----------------------------------------------------------------------
;; code-formatting.el
;;----------------------------------------------------------------------

;; adaptive fill for maintaining indentation inside comments

(defvar grail-group-filladapt-installer
  (grail-define-installer "filladapt" "file" "http://www.wonderworks.com/download/filladapt.el")
  "the installer for filladapt")

(grail-activate-with-recovery "code-formatting" filladapt grail-group-filladapt-installer

  (add-hook 'c-mode-common-hook 'c-setup-filladapt))

;; I would like to find a mode that lines up columns within code.

