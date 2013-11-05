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
    (dwim-tab-localize-context 'nxml-complete)
    (turn-on-dwim-tab 'nxml-indent-line)
    (configure-for-buffer-ring "nxml-mode"))
  t)



