;;----------------------------------------------------------------------
;; file-utilities
;;----------------------------------------------------------------------
(require 'cm-string)

(defun files-make-path-writable ( path )
  (set-file-modes path
    (file-modes-symbolic-to-number "u+w" (file-modes path))) )

(defun files-make-path-readonly ( path )
  (set-file-modes path
    (file-modes-symbolic-to-number "u-w" (file-modes path))) )

(defun files-filter-empty-strings ( path-list )
  (let
    ((filtered nil))

    (mapc
      (lambda ( dir )
        (when (> (length dir) 0)
          (setq filtered (cons dir filtered)) ))
      path-list)

    (reverse filtered) ))

(defun files-child-of-path ( root-path child-path )
  (catch 'done
    (let
      ((root-dirs  (files-filter-empty-strings (split-string root-path  "[/]")))
       (child-dirs (files-filter-empty-strings (split-string child-path "[/]"))))

      (while root-dirs
        (let
          ((next-root  (car root-dirs))
           (next-child (car child-dirs)))

          (when next-root
            (unless (and next-root next-child)
              (throw 'done nil))

            (unless (string-equal next-root next-child)
              (throw 'done nil))

            (setq root-dirs (cdr root-dirs))
            (setq child-dirs (cdr child-dirs)) ) ))

      (if child-dirs
        (concat (string-join "/" child-dirs) "/")
        nil) )))

;; (files-child-of-path "/home/codermattie/" "/home/codermattie/code/")
;; (files-child-of-path "/home/codermattie/" "/home/foob/code/")

(provide 'file-utilities)
