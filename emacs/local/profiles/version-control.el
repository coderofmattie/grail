;;----------------------------------------------------------------------
;; version-control.el
;;----------------------------------------------------------------------
(require 'cm-string)

(require 'vc)
(require 'merging)

(grail-load 'egg (grail-define-installer "egg"
                          "git"
                          "git://github.com/byplayer/egg.git"))

(setq
  vc-handled-backends `(Bzr SVN Git Hg Arch SCCS Mtn CVS RCS)
  vc-delete-logbuf-window t
  ;; just to be sure, this default may change in the future.
  vc-make-backup-files nil)

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
  (interactive)
  (catch 'no-log-files
    (let
      ((log-file (ver-ctl-log-select "choose log file: ")))

      (if log-file
        (switch-to-buffer
          (pop-to-buffer (find-file-noselect (cdr log-file)) nil t))
        (message "no log selected!")) )) )

(defun ver-ctl-log-file-create ( log-name )
  (interactive "Mname of log: ")

  (let
    ((log-file (concat (ver-ctl-log-dir buffer-file-name) "/" log-name ".log") ))

    (if (file-exists-p log-file)
      (message "log file %s already exists!" log-file)
      (progn
        (switch-to-buffer
          (pop-to-buffer (find-file-noselect log-file) nil t))) )) )

(defun ver-ctl-log-insert-label ()
  (interactive)
  (catch 'no-log-files
    (let
      ((log-file (ver-ctl-log-select "choose log label: ")))

      (if log-file
        (insert (concat "<" (car log-file) "> "))
        (message "no log selected!")) )))

(defun ver-ctl-log-insert-log ()
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
  (interactive)
  (message "branches: %s" (string-join "," (ver-ctl-branch-list))) )

(defun ver-ctl-diff ()
  (interactive)
  (ediff-revision buffer-file-name))

(defun ver-ctl-file-log ()
  (interactive)
  (egg-file-log buffer-file-name))

(defun ver-ctl-file-resolve ()
  (interactive)
  (egg-resolve-merge-with-ediff))

(defun ver-ctl-file-merge ()
  (interactive)
  (egg-log-buffer-merge))

(defun ver-ctl-execute ()
  (interactive)
  (call-interactively 'egg-next-action))

(defun ver-ctl-status ()
  (interactive)
  (call-interactively 'egg-status))

(defun ver-ctl-interface ()
  (interactive)
  (call-interactively 'egg-log))

(defun ver-ctl-repo ()
  (interactive)

  (setq current-prefix-arg 4)
  (call-interactively 'egg-log))

(defun ver-ctl-bindings ()
  (let
    ((ver-map (make-sparse-keymap)))

    (define-key ver-map "d" 'ver-ctl-diff)
    (define-key ver-map "l" 'ver-ctl-file-log)

    (define-key ver-map "v" 'ver-ctl-execute)
    (define-key ver-map "c" 'ver-ctl-resolve)
    (define-key ver-map "m" 'ver-ctl-merge)

    (define-key ver-map "s" 'ver-ctl-status)
    (define-key ver-map "x" 'ver-ctl-interface)
    (define-key ver-map "r" 'ver-ctl-repo)

    (local-set-key (kbd "C-c v") ver-map)))

(defun ver-ctl-hook ()
  (make-variable-buffer-local 'mattie-modeline-branch)
  (setq mattie-modeline-branch 'ver-ctl-branch-current)

  (ver-ctl-bindings)
  (ver-ctl-log-bindings))

(setq configure-programming-hook nil)

(add-hook 'configure-programming-hook 'ver-ctl-hook t)
