;;----------------------------------------------------------------------
;; code-formatting.el
;;----------------------------------------------------------------------

;; adaptive fill for maintaining indentation inside comments

(defvar grail-group-filladapt-installer
  (grail-define-installer "filladapt" "file" "http://www.wonderworks.com/download/filladapt.el")
  "the installer for filladapt")

(grail-activate-with-recovery "code-formatting" filladapt grail-group-filladapt-installer

  (add-hook 'emacs-lisp-mode-hook 'turn-on-filladapt-mode)

  (eval-after-load 'cc-mode    '(add-hook 'c-mode-common-hook   'c-setup-filladapt))
  (eval-after-load 'cperl-mode '(add-hook 'cperl-mode-hook      'turn-on-filladapt-mode))
  (eval-after-load 'lua-mode   '(add-hook 'lua-mode-hook        'turn-on-filladapt-mode))
  (eval-after-load 'cmuscheme  '(add-hook 'scheme-mode-hook     'turn-on-filladapt-mode))

  )



;; I would like to find a mode that lines up columns within code.

