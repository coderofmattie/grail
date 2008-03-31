;;----------------------------------------------------------------------
;; tab.el
;;----------------------------------------------------------------------
(require 'dwim-tab)

(add-hook 'emacs-lisp-mode-hook
  (lambda ()
    (dwim-tab-localize 'lisp-complete-symbol)))

(add-hook 'eshell-mode-hook
  (lambda ()
    ;; ensure that none of my custom keybindings are affected.
    (dwim-tab-localize 'pcomplete-expand-and-complete) ))

(eval-after-load "cperl-mode"
  (add-hook 'cperl-mode-hook 'dwim-tab-localize))
