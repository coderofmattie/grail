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
  "ctags-exuberant -V -e --links=yes -a -L -")

(defun tags-uber-find-initialize ( file-match tree-list )
  (string-join " "
    (cons "find"
      (append
        ;; necessary to keep find from barfing on symlinks
        (mapcar (lambda ( dir )
                  (concat dir "/"))  tree-list)
        (list (format " -iname '%s' -print |" file-match)))) ))

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

(defun tags-uber-builder-table-find ( tag-file )
  (catch 'hit
    (mapc
      (lambda ( entry )
        (when (string-equal tag-file (tags-uber-table-entry-file entry))
          (throw 'hit entry)) )
      tags-uber-builder-table)
    nil))

(defvar tags-uber-loaded-table nil)

(defun tags-uber-mode-and-name-to-global ( mode table-name )
  (concat mode "/" "table-name"))

;; (tags-uber-mode-and-name-to-global "cperl-mode" "imports")

(defun tags-uber-loaded-entry-create ( mode table-name table-file )
  (list nil mode table-name table-file))

;; (tags-uber-loaded-entry-create "cperl-mode" "imports" "table.tags")

(defun tags-uber-loaded-entry-status ( loaded-entry )
  (car loaded-entry))

;; (tags-uber-loaded-entry-status
;;   (tags-uber-loaded-entry-create "cperl-mode" "imports" "table.tags"))

(defun tags-uber-loaded-entry-mode ( loaded-entry )
  (nth 1 loaded-entry))

;; (tags-uber-loaded-entry-mode
;;   (tags-uber-loaded-entry-create "cperl-mode" "imports" "table.tags"))

(defun tags-uber-loaded-entry-name ( loaded-entry )
  (nth 2 loaded-entry))

;; (tags-uber-loaded-entry-name
;;   (tags-uber-loaded-entry-create "cperl-mode" "imports" "table.tags"))

(defun tags-uber-loaded-entry-file ( loaded-entry )
  (nth 3 loaded-entry))

;; (tags-uber-loaded-entry-file
;;   (tags-uber-loaded-entry-create "cperl-mode" "imports" "table.tags"))

(defun tags-uber-loaded-entry-mark-ready ( loaded-entry timestamp )
  (cons timestamp (cdr loaded-entry)) )

;; (tags-uber-loaded-entry-mark-ready
;;   (tags-uber-loaded-entry-create "cperl-mode" "imports" "table.tags"))

(defun tags-uber-loaded-entry-matches-p ( mode name loaded-entry )
  (if (and (string-equal mode (tags-uber-loaded-entry-mode loaded-entry))
           (string-equal name (tags-uber-loaded-entry-name loaded-entry)))
    t
    nil))

;; (tags-uber-loaded-entry-matches-p "cperl-mode" "imports"
;;   (tags-uber-loaded-entry-create "cperl-mode" "imports" "table.tags"))

(defun tags-uber-loaded-add ( new-entry )
  (setq tags-uber-loaded-table (cons new-entry tags-uber-loaded-table)) )

;; (tags-uber-loaded-add
;;   (tags-uber-loaded-entry-create "cperl-mode" "imports" "table.tags"))

;; tags-uber-loaded-table

(defun tags-uber-loaded-find ( mode table-name )
  (catch 'hit
    (mapc
      (lambda ( entry )
        (when (tags-uber-loaded-entry-matches-p mode table-name entry)
          (throw 'hit entry)) )
      tags-uber-loaded-table)
    nil))

;; (tags-uber-loaded-find "cperl-mode" "imports")

(defun tags-uber-create-timestamp ()
  (time-to-seconds (current-time)))

;; (tags-uber-create-timestamp)

(defun tags-uber-compare-timestamp ( left right )
  (if (> left right)
    t
    nil))

;; (tags-uber-compare-timestamp (tags-uber-create-timestamp) (tags-uber-create-timestamp))

;; (let
;;   ((earlier (tags-uber-create-timestamp)))

;;   (tags-uber-compare-timestamp (tags-uber-create-timestamp) earlier))

(defun tags-uber-loaded-mark-ready ( mode table-name )
  (let
    ((ready-entry nil))

    (mapcar
      (lambda ( entry )
        (when (tags-uber-loaded-entry-matches-p mode table-name entry)
          (setq ready-entry
            (tags-uber-loaded-entry-mark-ready entry (tags-uber-create-timestamp)))) )
      tags-uber-loaded-table)

    ready-entry))

;; (tags-uber-loaded-mark-ready "cperl-mode" "imports")

(defun tags-uber-loaded-delete ( mode table-name )
  (let
    ((new-loaded nil)
     (deleted-entry nil))

    (mapc
      (lambda ( entry )
        (if (tags-uber-loaded-entry-matches-p mode table-name entry)
          (setq deleted-entry entry)
          (setq new-loaded (cons entry new-loaded)) ))
      tags-uber-loaded-table)

    (when deleted-entry
      (setq tags-uber-loaded-table new-loaded))

    deleted-entry))

;; (tags-uber-loaded-delete "cperl-mode" "imports")

;; tags-uber-loaded-table

(defun tags-uber-loaded-entry-purge ( mode table-name )
  (let
    ((deleted (tags-uber-loaded-delete mode table-name)))

    (when deleted
      (tags-uber-delete-tags-file
        (tags-uber-loaded-entry-file deleted))) ))

(defun tags-uber-update-for-mode ( mode &rest tables )
  (let
    ((generator (tags-uber-get-command-generator mode)))

    (tags-uber-global-init)

    (catch 'abort
      (unless generator
        (message "tags uber: failed to find command generator for mode: %s" mode)
        (throw 'abort t))

      (mapc
        (lambda ( new-table )
          (let*
            ((table-name   (car new-table))
             (table-file   (tags-uber-tags-path mode table-name))
             (source-trees (cdr new-table))
             (commands     (funcall generator table-file source-trees)))

            (dolist ( source-dir source-trees )
              (unless (file-directory-p source-dir)
                (message "uber tags: source directory %s does not exist. aborting add." source-dir)
                (throw 'abort t)) )

            (tags-uber-loaded-entry-purge mode table-name)

            (tags-uber-builder-table-update
              (tags-uber-table-entry-create mode table-name table-file
                (car commands) (cdr commands)))

            (tags-uber-loaded-add
              (tags-uber-loaded-entry-create mode table-name table-file)) ))
        tables)
      nil)))

;; (setq tags-uber-builder-table nil)

;; (setq tags-uber-loaded-table nil)

;; (tags-uber-update-for-mode "cperl-mode"
;;    '("imports" "/home/codermattie/config" "/home/codermattie/code")
;;    '("exports" "/home/codermattie/config" "/home/codermattie/code"))

;; tags-uber-builder-table

;; tags-uber-loaded-table

(defun tags-uber-loaded-table-entry-compare ( left right )
  (message "got here")
  (catch 'returned
    (let
      ((left-status  (tags-uber-loaded-entry-status left))
       (right-status (tags-uber-loaded-entry-status right)))

      (when (not left-status) (throw 'returned left))
      (when (not right-status) (throw 'returned right))

      ;; return the oldest of the two
      (if (tags-uber-compare-timestamp left-status right-status)
        right
        left) )))

(defun tags-uber-loaded-table-sort ( table )
  (message "in sort")
  (sort table 'uber-loaded-table-entry-compare))

(defconst tags-uber-refresh-interval (* 60 15))

(defun tags-uber-loaded-entry-is-canidate ( table-entry )
  (catch 'canidate
    (let
      ((entry-status (tags-uber-loaded-entry-status table-entry)))

      (when (not entry-status)
        (throw 'canidate table-entry))

      (let
        ((entry-refresh-time (+ (truncate entry-status) tags-uber-refresh-interval)))
        (if (> (truncate (time-to-seconds (current-time))) entry-refresh-time)
          (table-entry)
          nil)) )))

(defun tags-uber-next-to-load ()
  (message "got there")

  (catch 'next-canidate
    (unless tags-uber-loaded-table
      (throw 'next-canidate nil))

    (when (equal 1 (length tags-uber-loaded-table))
      (throw 'next-canidate
        (if (tags-uber-loaded-entry-is-canidate (car tags-uber-loaded-table))
          (car tags-uber-loaded-table)
          nil)) )

    (mapc
      (lambda ( sorted-entry )
        (when (tags-uber-loaded-entry-is-canidate sorted-entry)
          (throw 'next-canidate sorted-entry)) )
      (tags-uber-loaded-table-sort (copy-list tags-uber-loaded-table)) )

    nil))

(defvar tags-uber-running-job nil)

(defun tags-uber-job-callback ( command status )
  (if status
    (progn
      (tags-uber-loaded-mark-ready
        (tags-uber-loaded-entry-mode tags-uber-running-job)
        (tags-uber-loaded-entry-name tags-uber-running-job)))

    (progn
      (message "uber tags: job failure! mode %s name %s command %s"
        (tags-uber-loaded-entry-mode tags-uber-running-job)
        (tags-uber-loaded-entry-name tags-uber-running-job)
        command)) )

  (setq tags-uber-running-job nil))

(defun tags-uber-queue-job ( loaded-entry )
  (catch 'abort
    (let
      ((builder-entry (tags-uber-builder-table-find (tags-uber-loaded-entry-file loaded-entry)) )
       (builder-command nil))

      (message "builder entry %s" (princ builder-entry) )
      (setq builder-command
        (if (tags-uber-loaded-entry-status loaded-entry)
          (tags-uber-table-entry-update builder-entry)
          (progn
            ;; sometimes on init a pre-existing file will cause a barf
            (when (file-readable-p (tags-uber-loaded-entry-file loaded-entry))
              (message "tags uber: purging old file %s" (tags-uber-loaded-entry-file loaded-entry))
              (delete-file (tags-uber-loaded-entry-file loaded-entry)))
            (tags-uber-table-entry-init builder-entry)) ))

      (unless builder-command
        (message "uber tags: cannot construct command for mode %s table %s"
          (tags-uber-loaded-entry-mode loaded-entry)
          (tags-uber-loaded-entry-name loaded-entry))
        (throw 'abort nil))

      (setq tags-uber-running-job loaded-entry)
      (cmd-queue-add-task builder-command 'tags-uber-job-callback)) t))

(defun tags-uber-reset-all ()
  (interactive)
  (setq
    tags-uber-running-job nil
    tags-uber-loaded-table nil
    tags-uber-builder-table nil))

(defun tags-uber-try-to-start-a-job ()
  (interactive)

  (catch 'exit-now
    (when tags-uber-running-job
      (throw 'exit-now nil))

    (unless tags-uber-loaded-table
      (throw 'exit-now nil))

    (let
      ((runnable (tags-uber-next-to-load)))
      (message "got to run")

      (if runnable
        (tags-uber-queue-job runnable)
        nil)) ))

(defvar tags-uber-loaded-for-mode nil)

(defun tags-uber-loaded-for-this-mode ()
  (and tags-uber-loaded-for-mode
       (string-equal major-mode tags-uber-loaded-for-mode)))

(defun tags-uber-load-tags-for-mode ()
  (interactive)

  (catch 'done

    (mapcar
      (lambda (loaded-entry)
        (when (and
                (string-equal major-mode (tags-uber-loaded-entry-mode loaded-entry))
                (tags-uber-loaded-entry-status loaded-entry))
          (visit-tags-table (tags-uber-loaded-entry-file loaded-entry))
          (throw 'done t) ))
      tags-uber-loaded-table)
    nil))

(defun tags-uber-switch-for-mode ()
  (if tags-uber-global-ready
    (or (tags-uber-loaded-for-this-mode)
        (tags-uber-load-tags-for-mode))
    nil))

(defadvice switch-to-buffer (after tags-switch)
  (tags-uber-switch-for-mode)
  ad-return-value)

(defvar tags-uber-global-ready nil)

(defun tags-uber-global-ready-p ()
  tags-uber-global-ready)

(defun tags-uber-global-init ()
  (unless tags-uber-global-ready
    (setq tags-uber-global-ready t)
    (ad-activate 'switch-to-buffer)) )

(provide 'tags-uber)
