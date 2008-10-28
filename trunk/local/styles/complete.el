;;----------------------------------------------------------------------
;; complete.el
;;----------------------------------------------------------------------

(require 'dwim-tab)

;; icicles is not just tab completion. It is a powerful experiment
;; in solving some fundamental problems in computer human interaction.
;; This rabbit hole is worthy of a book.

(robust-load-elisp "icicles"
  (require 'icicles)

  (setq
    icicle-generic-S-tab-keys (cons (kbd "<C-tab>") nil)   ;; I use S-tab already
    icicle-customize-save-flag nil                         ;; disable auto-save of customize
    )

  (icy-mode))
