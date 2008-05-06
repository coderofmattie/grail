;;----------------------------------------------------------------------
;;                    xml/html
;;----------------------------------------------------------------------
(require 'nxml-mode)

(setq auto-mode-alist (append '( ("\\.html$"    . nxml-mode)
                                 ("\\.xhtml$"   . nxml-mode)
                                 ("\\.xml$"     . nxml-mode)
                                 ) auto-mode-alist ))



