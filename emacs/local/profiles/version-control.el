;;----------------------------------------------------------------------
;; version-control.el
;;----------------------------------------------------------------------
(require 'subr-x)
(require 'custom-key)

(require 'vc)
(require 'merging)

;; egg fails with a busted autoload without this require
(require 'edmacro)

(grail-load-package 'egg "git" "git://github.com/byplayer/egg.git")

(grail-load-package 'ahg "hg" "https://bitbucket.org/agriggio/ahg" )

(setq
  vc-handled-backends `(Bzr SVN Git Hg Arch SCCS Mtn CVS RCS)
  vc-delete-logbuf-window t
  ;; just to be sure, this default may change in the future.
  vc-make-backup-files nil)

(remove-hook 'ediff-quit-hook 'egg--kill-ediffing-temp-buffers)

;;----------------------------------------------------------------------
;; library functions based on various vcs modes
;;----------------------------------------------------------------------

(defun ver-ctl-root ( from-path )
  (or
    (vc-find-root from-path ".git")
    (vc-find-root from-path ".hg") ))

(defun ver-ctl-branch-list ()
  (let*
    ((vc-fileset (vc-deduce-fileset t))
     (backend (car vc-fileset)))

    (funcall (vc-find-backend-function backend 'branches)) ))

(defun ver-ctl-branch-current ()
  (car (ver-ctl-branch-list)))

;;----------------------------------------------------------------------
;; logging stuff
;;----------------------------------------------------------------------
(require 'log-mode)

(defun ver-ctl-log-dir ( &optional path )
  ;; use a path for code buffers. egg sets default-directory
  ;; in log buffers.
  (dir-path-if-accessible (expand-file-name
                            (concat
                              (if path
                                (ver-ctl-root path)
                                default-directory)
                              "/logs/"))) )

(defun ver-ctl-log-dir-normal ()
  (ver-ctl-log-dir buffer-file-name))

(defun ver-ctl-log-dir-logmode ()
  (ver-ctl-dir))

(defvar ver-ctl-log-dir-locate 'ver-ctl-log-dir-normal)

(defun ver-ctl-log-files ()
  (let
    ((log-dir (funcall ver-ctl-log-dir-locate)))

    (when log-dir
      (directory-files log-dir t "\\.log$")) ))

(defun ver-ctl-log-file-pairs ( paths )
  (when paths
    (mapcar
      (lambda ( path )
        (cons (file-name-base path) path))
      paths)))

(defun ver-ctl-log-completion ( prompt log-pairs )
  (let
    ((input (completing-read
              prompt (mapcar 'car log-pairs))))
    (if (equal 0 (length input))
      nil
      (assoc input log-pairs)) ))

(defun ver-ctl-log-file-list ()
  "list log files"
  (interactive)
  (message "log files: %s"
    (string-join  (mapcar 'car (ver-ctl-log-file-pairs (ver-ctl-log-files))) ",")) )

(defun ver-ctl-log-select ( prompt )
  (let
    ((existing-logs (ver-ctl-log-file-pairs (ver-ctl-log-files)) ))

    (if existing-logs
      (ver-ctl-log-completion prompt existing-logs)
      (progn
        (message "no log files found! ")
        (throw 'no-log-files nil)) ) ))

(defun ver-ctl-log-file-open ()
  "open log"
  (interactive)
  (catch 'no-log-files
    (let
      ((log-file (ver-ctl-log-select "choose log file: ")))

      (if log-file
        (switch-to-buffer
          (pop-to-buffer (find-file-noselect (cdr log-file)) nil t))
        (message "no log selected!")) )) )

(defun ver-ctl-log-file-create ( log-name )
  "create log"
  (interactive "Mname of log: ")

  (let
    ((log-file (concat (ver-ctl-log-dir buffer-file-name) "/" log-name ".log") ))

    (if (file-exists-p log-file)
      (message "log file %s already exists!" log-file)
      (progn
        (switch-to-buffer
          (pop-to-buffer (find-file-noselect log-file) nil t))) )) )

(defun ver-ctl-log-insert-label ()
  "insert label"
  (interactive)
  (catch 'no-log-files
    (let
      ((log-file (ver-ctl-log-select "choose log label: ")))

      (if log-file
        (insert (concat "<" (car log-file) "> "))
        (message "no log selected!")) )))

(defun ver-ctl-log-insert-log ()
  "insert log as msg"
  (interactive)
  (catch 'no-log-files
    (let
      ((log-file (ver-ctl-log-select "choose log file: ")))

      (if log-file
        (progn
          (insert-file-contents (cdr log-file))

          (when (yes-or-no-p "delete log after insert? ")
            (delete-file (cdr log-file))
            (message "log: %s deleted" (car log-file))) )

        (message "no log selected!")) )))

(defun ver-ctl-log-bindings ()
  (custom-key-group "code editing" "l" nil
    ("l" . ver-ctl-log-file-list)
    ("c" . ver-ctl-log-file-create)
    ("o" . ver-ctl-log-file-open)
    ("i" . ver-ctl-log-insert-label)
    ("m" . ver-ctl-log-insert-log)) )

(defvar ver-ctl-egg-log-hook nil)

(defadvice egg-commit-log-edit ( after ver-ctl/log-edit-hook )
  (run-custom-hooks ver-ctl-egg-log-hook)
  ad-return-value)

(ad-activate 'egg-commit-log-edit)

(add-hook 'ver-ctl-egg-log-hook
  (lambda ()
    (ver-ctl-log-bindings))
  t)

;;----------------------------------------------------------------------
;; commands
;;----------------------------------------------------------------------

(defun ver-ctl-vc-branch-list ()
  "ver-ctl-vc-branch-list: show all branches in repo"
  (interactive)
  (message "branches: %s" (string-join "," (ver-ctl-branch-list))) )

(defun ver-ctl-git-diff ()
  "ver-ctl-git-diff: diff file against version (git)"
  (interactive)
  (merging-ediff-teardown-diff-egg-toggle-enable)
  (call-interactively 'ediff-revision buffer-file-name))

(defun ver-ctl-vc-diff ()
  "ver-ctl-vc-diff: diff file against version (vc)"
  (interactive)
  (call-interactively 'ediff-revision buffer-file-name))

(defun ver-ctl-git-file-log ()
  "ver-ctl-git-file-log: file log (git)"
  (interactive)
  (egg-file-log buffer-file-name))

(defun ver-ctl-ahg-file-log ()
  "ver-ctl-ahg-file-log: file log (hg)"
  (interactive)
  (ahg-log-cur-file buffer-file-name) )

(defun ver-ctl-git-short-log ()
  "ver-ctl-git-short-log: short log (git)"
  (interactive)
  (call-interactively 'egg-log))

(defun ver-ctl-ahg-short-log ()
  "ver-ctl-ahg-short-log: short log (hg)"
  (interactive)
  (ahg-short-log "tip" 0) )

(defun ver-ctl-git-file-blame ()
  "ver-ctl-git-file-blame: toggle blame mode (git)"
  (interactive)
  (egg-file-toggle-blame-mode))

(defun ver-ctl-git-file-resolve ()
  "ver-ctl-file-resolve: mark file as resolved (git)"
  (interactive)
  (egg-resolve-merge-with-ediff))

(defun ver-ctl-git-file-merge ()
  "ver-ctl-file-merge: merge conflicted file (git)"
  (interactive)
  (egg-log-buffer-merge))

(defun ver-ctl-git-status ()
  "ver-ctl-git-status: show tree status (git)"
  (interactive)
  (call-interactively 'egg-status))

(defun ver-ctl-ahg-status ()
  "ver-ctl-ahg-status: show tree status (hg)"
  (interactive)
  (call-interactively 'ahg-status))

(defun ver-ctl-git-repository ()
  "ver-ctl-git-repo: show the entire repository (git)"
  (interactive)

  (setq current-prefix-arg 4)
  (call-interactively 'egg-log))

(defun ver-ctl-git-execute ()
  "ver-ctl-git-execute: perform the next version control action"
  (interactive)
  (call-interactively 'egg-next-action))

(defun ver-ctl-ahg-execute ()
  "ver-ctl-git-execute: perform the next version control action"
  (interactive)
  (call-interactively 'ahg-do-command))

(defun ver-ctl-vc-revert ()
  "ver-ctl-vc-revert: revert the file back to the commit state"
  (interactive)
  (vc-revert-file buffer-file-name))

(defvar ver-ctl-vc-table
  `(("vc-branches"  . ver-ctl-vc-branch-list)

    ("vc-diff"      . ver-ctl-ahg-diff)
    ("git-diff"     . ver-ctl-git-diff)

    ("hg-log"  . ver-ctl-ahg-file-log)
    ("git-log" . ver-ctl-git-file-log)

    ("hg-short-log"  . ver-ctl-ahg-short-log)
    ("git-short-log" . ver-ctl-git-short-log)

    ("hg-blame"     . ver-ctl-ahg-file-blame)
    ("git-blame"    . ver-ctl-git-file-blame)

    ("git-resolve"  . ver-ctl-git-file-resolve)

    ("git-merge"    . ver-ctl-git-file-merge)

    ("git-status"   . ver-ctl-git-status)
    ("hg-status"    . ver-ctl-ahg-status)

    ("git-execute"  . ver-ctl-git-br-execute)
    ("hg-execute"   . ver-ctl-ahg-br-execute)

    ("git-repository"  . ver-ctl-git-repository)

    ("vc-revert" . ver-ctl-vc-revert) ))

(defun ver-ctl-vc-name ()
  (let
    ((vc-for-buffer (symbol-name (vc-backend buffer-file-name))))

    (if vc-for-buffer
      (downcase vc-for-buffer)
      nil) ))

(defun ver-ctl-call-function ( func )
  (let
    ((lookup
       (or (assoc (concat (ver-ctl-vc-name) "-" func) ver-ctl-vc-table)
           (assoc (concat "vc-" func) ver-ctl-vc-table)) ))

    (if lookup
      (progn
        (call-interactively (cdr lookup)))
      (message "version control: %s does not implement %s"
        (vc-backend buffer-file-name)
        func)) ))

(defun ver-ctl-bindings ()
  (let
    ((ver-map (make-sparse-keymap)))

    (define-key ver-map "b"
      (lambda ()
        "show all branches"
        (interactive)
        (ver-ctl-call-function "branches")) )

    (define-key ver-map "d"
      (lambda ()
        "diff the current file against head"
        (interactive)
        (ver-ctl-call-function "diff")) )

    (define-key ver-map "f"
      (lambda ()
        "file log"
        (interactive)
        (ver-ctl-call-function "log")) )

    (define-key ver-map "l"
      (lambda ()
        "short log"
        (interactive)
        (ver-ctl-call-function "short-log")) )

    (define-key ver-map "a"
      (lambda ()
        "blame mode"
        (interactive)
        (ver-ctl-call-function "blame")) )

    (define-key ver-map "e"
      (lambda ()
        "execute command"
        (interactive)
        (ver-ctl-call-function "execute")) )

    (define-key ver-map "r"
      (lambda ()
        "mark resolved"
        (interactive)
        (ver-ctl-call-function "resolve")) )

    (define-key ver-map "m"
      (lambda ()
        "merge"
        (interactive)
        (ver-ctl-call-function "merge")) )

    (define-key ver-map "s"
      (lambda ()
        "status"
        (interactive)
        (ver-ctl-call-function "status")) )

    (define-key ver-map "x"
      (lambda ()
        "repository view"
        (interactive)
        (ver-ctl-call-function "repository")) )

    (define-key ver-map "u"
      (lambda ()
        "revert file"
        (interactive)
        (ver-ctl-call-function "revert")) )

    (define-key ver-map "h" (keybindings-help-local "ver ctl" ver-map))

    (custom-key-group-register "v" "version control" ver-map)

    (local-set-key (kbd "C-c v") ver-map) ))

(defun ver-ctl-modeline-string ()
  (let
    ((detect-vcs  (condition-case nil
                    (vc-backend buffer-file-name)
                    (error "vc?")) )

     (detect-branch (condition-case nil
                      (ver-ctl-branch-current)
                      (error "?"))) )

    (format "%s -> %s" detect-vcs detect-branch) ))

(defun ver-ctl-hook ()
  (make-variable-buffer-local 'mattie-modeline-vcs)
  (setq mattie-modeline-vcs   'ver-ctl-modeline-string)

  (ver-ctl-bindings)
  (ver-ctl-log-bindings))

(add-hook 'configure-programming-hook 'ver-ctl-hook t)

(provide 'profile/version-control)

