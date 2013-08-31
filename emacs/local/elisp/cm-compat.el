;; define-error originated in XEmacs. This implementation shares the
;; same name, but not the interface. I need to clone or copy the
;; XEmacs version.

(defmacro define-error ( symbol message &rest isa-list )
  "define a error symbol with a isa list and a error message"
  `(progn
     (put ',symbol
       'error-conditions (append '(error ,symbol) ',isa-list))
     (put ',symbol 'error-message ,message) ))

(provide 'cm-compat)
