;;----------------------------------------------------------------------
;; sql group
;;----------------------------------------------------------------------

;; (grail-activate-with-recovery

(defvar grail-groups-plsql-installer
  (grail-define-installer "plsql" "file"
    "http://www.emacswiki.org/cgi-bin/wiki/download/plsql.el")
  "the plsql installer")

(grail-activate-with-recovery "sql" plsql grail-groups-plsql-installer
  (setq plsql-indent 2))
