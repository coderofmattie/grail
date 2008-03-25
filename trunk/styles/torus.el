;;----------------------------------------------------------------------
;; torus.el
;;----------------------------------------------------------------------
(require 'mtorus)

(mtorus-enable-select-follows-choose)

(add-hook 'mtorus-navigate-post-hook
  (lambda (element)
    (mtorus-select-element element)))

(global-set-key (kbd "<M-right>")  'mtorus-next-element)
(global-set-key (kbd "<M-left>")   'mtorus-prev-element)

(global-set-key (kbd "<M-up>")     'mtorus-parent-element)
(global-set-key (kbd "<M-down>")   'mtorus-child-element)

(global-set-key (kbd "M-RET")      'mtorus-select-element)




