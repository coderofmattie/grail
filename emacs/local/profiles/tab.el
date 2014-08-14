;;----------------------------------------------------------------------
;; tab.el
;;----------------------------------------------------------------------
(grail-load-package 'hippie-exp "pkg")

(grail-load-package 'hippie-namespace "pkg")

(dwim-tab-globalize-context (dwim-tab-make-expander 'dwim-tab-stem-trigger 'hippie-expand))

(provide 'profile/tab)
