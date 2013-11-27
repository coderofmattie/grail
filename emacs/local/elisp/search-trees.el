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

(defun search-trees-get-for-host-and-mode ( host mode )
  (let
    ((all-trees nil)
     (host (system-name)))

    (mapc
      (lambda ( host-tree )
        (when (and (string-equal host (car host-tree))
                   (string-equal mode (cadr host-tree)))
          (let
            ((table (nth 2 host-tree)))
            (setq all-trees (cons (cons (car table) (cdr table)) all-trees)) ) ))
      search-trees-global-host)

    all-trees))

(provide 'search-trees)
