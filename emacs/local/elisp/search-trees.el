;;----------------------------------------------------------------------
;; search-trees
;;----------------------------------------------------------------------
(defvar search-trees-global-host nil)

(defvar search-trees-global-user nil)

(defun search-trees-add-for-host ( host mode &rest tree-list )
  (dolist (next-tree tree-list)
    (setq search-trees-global-host (cons (list host mode next-tree) search-trees-global-host)) ))

(defun search-trees-add-for-user ( user mode &rest tree-list )
  (dolist (next-tree tree-list)
    (setq search-trees-global-user (cons (list user mode next-tree) search-trees-global-user)) ))

(defun search-trees-get-for-key ( key mode search-table)
  (let
    ((all-trees nil))

    (mapc
      (lambda ( tree-entry )
        (when (and (string-equal key (car tree-entry))
                   (string-equal mode (cadr tree-entry)))
          (let
            ((table (nth 2 tree-entry)))
            (setq all-trees (cons (cons (car table) (cdr table)) all-trees)) ) ))
      search-table)
    all-trees))

(defun search-trees-get-for-host-and-user ( mode )
  (append
    (search-trees-get-for-key (user-login-name) mode search-trees-global-user)
    (search-trees-get-for-key (system-name) mode search-trees-global-host)) )

(provide 'search-trees)
