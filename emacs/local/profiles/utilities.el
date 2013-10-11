;;----------------------------------------------------------------------
;; utilities.el
;; Primary Author: Mike Mattie
;; Copyright: Mike Mattie 2013
;;----------------------------------------------------------------------

(grail-load 'hippie-exp (grail-define-installer "hippie-exp"
                          "pkg"
                          'hippie-exp))

(grail-load 'hippie-namespace  (grail-define-installer "hippie-namespace"
                          "pkg"
                          'hippie-namespace))

(global-hippie-namespace-mode 1)

(global-set-key (kbd "C-c TAB")  'hippie-expand)



