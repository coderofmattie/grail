;;----------------------------------------------------------------------
;; tags-comprehensive.el
;;
;; comprehensive tags support
;;----------------------------------------------------------------------
(require 'command-queue)
(require 'cm-string)

(defvar tags-uber-tags-dir (concat (getenv "HOME") "/code/tags"))

(defun tags-uber-tags-path ( mode table-name )
  (concat tags-uber-tags-dir "/" mode "-" table-name))

(tags-uber-tags-path "cperl-mode" "imports")

(defun tags-uber-check-tags-dir ()
  (unless (file-directory-p tags-uber-tags-dir)
    (make-directory tags-uber-tags-dir t)) )

;; (tags-uber-check-tags-dir)

(defun tags-uber-delete-tags-file ( tags-file )
  (when (file-readable-p tags-file)
    (delete-file tags-file)))

;; (tags-uber-delete-tags-file (tags-uber-tags-path "cperl-mode" "imports"))

(defconst tags-uber-cmd-base
  "ctags-exuberant -e --file-scope=yes --links=yes --sort=yes --append=no -L -")

(defun tags-uber-find-initialize ( file-match tree-list )
  (string-join " "
    (cons "find"
      (append
        tree-list
        (list (format "-iname '%s' -print |" file-match)))) ))

;; (tags-uber-find-initialize "*.p[ml]" '("/home/foo" "/home/bar"))

(defun tags-uber-find-update ( tags-file file-match tree-list )
  (string-join " "
    (cons "find"
      (append
        tree-list
        (list (format "-newerm %s -iname '%s' -print |" tags-file file-match)))) ))

;; (tags-uber-find-update "/foo-tags" "*.p[ml]" '("/home/foo" "/home/bar"))

(defun tags-uber-tags-command-builder ( lang tags-file file-match tree-list )
  (cons
    (concat
      (tags-uber-find-initialize file-match tree-list)
      " "
      (string-join " "
        (list
          tags-uber-cmd-base
          (format "--languages=%s" lang)
          (format "-f %s" tags-file))) )

    (concat
      (tags-uber-find-update tags-file "*.p[ml]" tree-list)
      " "
      (string-join " "
        (list
          tags-uber-cmd-base
          (format "--languages=%s" lang)
          (format "-f %s" tags-file))) ) ))

;; (tags-uber-tags-command-builder "Perl" "/foo-tags" "*.p[ml]" '("/home/foo" "/home/bar"))

(defconst tags-uber-cmd-generators
  '(("cperl-mode" (lambda ( tags-file tree-list )
                    (tags-uber-tags-command-builder "Perl" tags-file  "*.p[ml]" tree-list)))
     ))

(defun tags-uber-get-command-generator ( mode )
  (catch 'found
    (mapc (lambda ( generator )
            (when (string-equal (car generator) mode)
              (throw 'found (cadr generator)) ))
      tags-uber-cmd-generators)
    nil))

;; (funcall
;;   (tags-uber-get-command-generator "cperl-mode")
;;   "/foo-tags" '("/home/foo" "/home/bar"))

(defun tags-uber-table-entry-create ( mode table-name table-file create update )
  (list mode table-name table-file create update))

(defun tags-uber-table-entry-mode ( table-entry )
  (car table-entry))

(defun tags-uber-table-entry-name ( table-entry )
  (nth 1 table-entry))

(defun tags-uber-table-entry-file ( table-entry )
  (nth 2 table-entry))

(defun tags-uber-table-entry-init ( table-entry )
  (nth 3 table-entry))

(defun tags-uber-table-entry-update ( table-entry )
  (nth 4 table-entry))

(defun tags-uber-table-entry-same-p ( left right )
  (if (and
        (string-equal (tags-uber-table-entry-mode left) (tags-uber-table-entry-mode right))
        (string-equal (tags-uber-table-entry-name left) (tags-uber-table-entry-name right)) )
    t
    nil))

;; (tags-uber-table-entry-create "cperl-mode" "imports" "imports.tags" "create" "update")

;; (tags-uber-table-entry-mode
;;   (tags-uber-table-entry-create "cperl-mode" "imports" "imports.tags" "create" "update"))

;; (tags-uber-table-entry-name
;;   (tags-uber-table-entry-create "cperl-mode" "imports" "imports.tags" "create" "update"))

;; (tags-uber-table-entry-file
;;   (tags-uber-table-entry-create "cperl-mode" "imports" "imports.tags" "create" "update"))

;; (tags-uber-table-entry-init
;;   (tags-uber-table-entry-create "cperl-mode" "imports" "imports.tags" "create" "update"))

;; (tags-uber-table-entry-same-p
;;   (tags-uber-table-entry-create "cperl-mode" "imports" "imports.tags" "create" "update")
;;   (tags-uber-table-entry-create "cperl-mode" "imports" "imports.tags" "create" "update"))

(tags-uber-table-entry-same-p
  (tags-uber-table-entry-create "cperl-mode" "imports" "imports.tags" "create" "update")
  (tags-uber-table-entry-create "cperl-mode" "exports" "imports.tags" "create" "update"))

(defvar tags-uber-builder-table '())

(defun tags-uber-builder-table-update ( new-entry )
  (let
    ((updated-table nil)
     (did-update nil))

    (mapc
      (lambda (existing-entry)
        (setq updated-table
          (cons
            (if (tags-uber-table-entry-same-p existing-entry new-entry)
              (progn
                (setq did-update t)
                new-entry)
              existing-entry)
            updated-table)))
      tags-uber-builder-table)

    (if did-update
      (tags-uber-delete-tags-file (tags-uber-table-entry-file new-entry))
      (setq updated-table
        (cons new-entry updated-table)))

    (setq tags-uber-builder-table updated-table)

    did-update))

;; (tags-uber-builder-table-update
;;   (tags-uber-table-entry-create "cperl-mode" "imports" "imports.tags" "create" "update"))

;; (tags-uber-builder-table-update
;;   (tags-uber-table-entry-create "cperl-mode" "exports" "imports.tags" "create" "update"))

;; tags-uber-builder-table

;; (tags-uber-builder-table-update
;;   (tags-uber-table-entry-create "cperl-mode" "exports" "imports.tags" "create" "update"))

(defvar tags-uber-loaded-table nil)

(defun tags-uber-mode-and-name-to-global ( mode table-name )
  (concat mode "/" "table-name"))

;; (tags-uber-mode-and-name-to-global "cperl-mode" "imports")

(defun tags-uber-loaded-entry-create ( mode table-name table-file )
  (list nil mode table-name table-file))

;; (tags-uber-loaded-entry-create "cperl-mode" "imports" "table.tags")

(defun tags-uber-loaded-entry-status ( loaded-entry )
  (car loaded-entry))

(tags-uber-loaded-entry-status
  (tags-uber-loaded-entry-create "cperl-mode" "imports" "table.tags"))

(defun tags-uber-loaded-entry-mode ( loaded-entry )
  (nth)
  )
(defun tags-uber-loaded-entry-mark-loaded ( loaded-entry )
  (cons t (cdr loaded-entry)) )

(tags-uber-loaded-entry-mark-loaded
  (tags-uber-loaded-entry-create "cperl-mode" "imports" "table.tags"))


(defun tags-uber-loaded-entry-invalidate ( loaded-entry )
  (cons t (cdr loaded-entry))
  )

(defun tags-uber-loaded-table-update ( new-entry )


  )

(defun tags-uber-builder-table-update-for-mode ( mode &rest tables )
  (let
    ((generator (tags-uber-get-command-generator mode)))

    (catch 'abort
      (unless generator
        (message "tags uber: failed to find command generator for mode: %s" mode)
        (throw 'abort t))

      (mapc
        (lambda ( new-table )
          (let*
            ((table-file (tags-uber-tags-path mode (car new-table)))
             (commands
               (funcall generator table-file (cdr new-table))))

          (tags-uber-builder-table-update
            (tags-uber-table-entry-create mode (car new-table) table-file
              (car commands) (cdr commands))) ))
        tables) )))


;; (setq tags-uber-builder-table nil)

;; tags-uber-builder-table

;; (tags-uber-builder-table-update-for-mode "cperl-mode"
;;   '("imports" "src" "test" "docs")
;;   '("exports" "src" "test" "docs"))

;; (tags-uber-builder-table-update
;;   (tags-uber-table-entry-create "cperl-mode" "exports" "imports.tags" "create" "update"))

;; (tags-uber-create-tables-for-language
;;   '("foo" "foo-source" "bar-source" "baz-source")
;;   '("bingo" "bingo-source"))


(provide 'tags-uber)
