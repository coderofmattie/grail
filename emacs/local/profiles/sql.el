;;----------------------------------------------------------------------
;; sql group
;;----------------------------------------------------------------------
(grail-load 'plsql (grail-define-installer
                     "plsql"
                     "file"
                     "http://www.emacswiki.org/cgi-bin/wiki/download/plsql.el"))

(setq plsql-indent 2)
