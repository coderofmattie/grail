;;----------------------------------------------------------------------
;; tab.el
;;----------------------------------------------------------------------
(grail-load 'hippie-exp (grail-define-installer "hippie-exp"
                          "pkg"
                          'hippie-exp))

(grail-load 'hippie-namespace  (grail-define-installer "hippie-namespace"
                          "pkg"
                          'hippie-namespace))

(dwim-tab-globalize-context (dwim-tab-make-expander 'dwim-tab-stem-trigger 'hippie-expand))
