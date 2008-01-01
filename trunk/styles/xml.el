;;----------------------------------------------------------------------
;;                    xml/html
;;----------------------------------------------------------------------
(require 'nxml-mode)

(setq auto-mode-alist (append '( ("\\.html$"    . nxml-mode)
                                 ("\\.xhtml$"   . nxml-mode)
                                 ("\\.xml$"     . nxml-mode)
                                 ) auto-mode-alist ))

(add-hook 'nxml-mode-hook
  (lambda ()
    ;; the allout mode keybindings are found with C-c C-h
    (apply-my-keybindings 'nxml-complete)
    ))

