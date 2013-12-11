;;----------------------------------------------------------------------
;; version-control.el
;;----------------------------------------------------------------------
(require 'cm-string)

(require 'vc)
(require 'merging)

;; egg fails with a busted autoload without this require
(require 'edmacro)

(grail-load 'egg (grail-define-installer "egg"
                          "git"
                          "git://github.com/byplayer/egg.git"))

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
  (vc-find-root from-path ".git"))

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
    (string-join "," (mapcar 'car (ver-ctl-log-file-pairs (ver-ctl-log-files))))) )

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
  (let
    ((ver-map (make-sparse-keymap)))

    (define-key ver-map "l" 'ver-ctl-log-file-list)
    (define-key ver-map "c" 'ver-ctl-log-file-create)
    (define-key ver-map "o" 'ver-ctl-log-file-open)
    (define-key ver-map "i" 'ver-ctl-log-insert-label)
    (define-key ver-map "m" 'ver-ctl-log-insert-log)

    (define-key ver-map "h" (keybindings-help-fn "ver log" ver-map))
    (local-set-key (kbd "C-c l") ver-map)))

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

(defun ver-ctl-branch-show-all ()
  "ver-ctl-branch-show-all: show all branches in repo"
  (interactive)
  (message "branches: %s" (string-join "," (ver-ctl-branch-list))) )

(defun ver-ctl-diff ()
  "ver-ctl-file-diff: diff file against version"
  (interactive)
  (merging-ediff-teardown-diff-egg-toggle-enable)
  (call-interactively 'ediff-revision buffer-file-name))

(defun ver-ctl-file-log ()
  "ver-ctl-file-log: diff file against version"
  (interactive)
  (egg-file-log buffer-file-name))

(defun ver-ctl-file-blame ()
  "ver-ctl-file-blame: toggle blame mode"
  (interactive)
  (egg-file-toggle-blame-mode))

(defun ver-ctl-file-resolve ()
  "ver-ctl-file-resolve: mark file as resolved"
  (interactive)
  (egg-resolve-merge-with-ediff))

(defun ver-ctl-file-merge ()
  "ver-ctl-file-merge: merge conflicted file"
  (interactive)
  (egg-log-buffer-merge))

(defun ver-ctl-execute ()
  "ver-ctl-execute: perform the next version control action"
  (interactive)
  (call-interactively 'egg-next-action))

(defun ver-ctl-status ()
  "ver-ctl-status: show version control tree status"
  (interactive)
  (call-interactively 'egg-status))

(defun ver-ctl-interface ()
  "ver-ctl-interface: show version control interface"
  (interactive)
  (call-interactively 'egg-log))

(defun ver-ctl-repo ()
  "ver-ctl-repo: show the entire repository"
  (interactive)

  (setq current-prefix-arg 4)
  (call-interactively 'egg-log))

(defun ver-ctl-bindings ()
  (let
    ((ver-map (make-sparse-keymap)))

    (define-key ver-map "d" 'ver-ctl-diff)
    (define-key ver-map "l" 'ver-ctl-file-log)
    (define-key ver-map "b" 'ver-ctl-file-blame)

    (define-key ver-map "v" 'ver-ctl-execute)
    (define-key ver-map "c" 'ver-ctl-file-resolve)
    (define-key ver-map "m" 'ver-ctl-file-merge)

    (define-key ver-map "s" 'ver-ctl-status)
    (define-key ver-map "x" 'ver-ctl-interface)
    (define-key ver-map "r" 'ver-ctl-repo)

    (define-key ver-map "h" (keybindings-help-fn "ver ctl" ver-map))

    (local-set-key (kbd "C-c v") ver-map)))


(defun ver-ctl-modeline-string ()
  (let
    ((detect-vcs (vc-backend buffer-file-name)))

    (if detect-vcs
      (concat (format "%s" detect-vcs) "->" (ver-ctl-branch-current))
      "") ))

(defun ver-ctl-hook ()
  (make-variable-buffer-local 'mattie-modeline-vcs)
  (setq mattie-modeline-vcs   'ver-ctl-modeline-string)

  (ver-ctl-bindings)
  (ver-ctl-log-bindings))

(add-hook 'configure-programming-hook 'ver-ctl-hook t)
