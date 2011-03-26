;;----------------------------------------------------------------------
;; tab.el
;;----------------------------------------------------------------------
(require 'dwim-tab)

(defun template-localize-tab ( &rest complete )
  (apply 'dwim-tab-localize
    (append
      (when (templates-enabled-p)
        (dwim-tab-local-context 'template/next)
        (list 'template/expand))
      complete)))

;; emacs builtin modes

(add-hook 'emacs-lisp-mode-hook
  (lambda ()
    (template-localize-tab 'lisp-complete-symbol))
  t)

(add-hook 'eshell-mode-hook
  (lambda ()
    ;; ensure that none of my custom keybindings are affected.
    (dwim-tab-localize 'pcomplete-expand-and-complete) ))

;; setup for modes that are not automatically loaded.
(eval-after-load 'scheme-mode
  ;; there is no scheme completion that I know of ... fsck!
  '(add-hook 'scheme-mode-hook 'template-localize-tab t))

(eval-after-load 'nxml-mode
  '(add-hook 'nxml-mode-hook
     (lambda ()
       (template-localize-tab 'nxml-complete))
     t))

(eval-after-load 'cperl-mode
  '(add-hook 'cperl-mode-hook 'template-localize-tab t))



