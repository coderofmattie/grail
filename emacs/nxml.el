;;----------------------------------------------------------------------
;;                    xml/html
;;----------------------------------------------------------------------
(require 'nxml-mode)

(setq auto-mode-alist (append '( ("\\.html$"    . nxml-mode)
                                 ("\\.xhtml$"   . nxml-mode)
                                 ("\\.xml$"     . nxml-mode)
                                 ) auto-mode-alist ))

(defun nxml-configure-for-mode ()
  (buffer-ring/add "nxml-mode")
  (buffer-ring/local-keybindings)

  (dwim-tab-localize-context (dwim-tab-make-expander
                               'dwim-tab-stem-trigger
                               'nxml-complete))

  (turn-on-dwim-tab 'nxml-indent-line) )

(add-hook 'nxml-mode-hook 'nxml-configure-for-mode t)

