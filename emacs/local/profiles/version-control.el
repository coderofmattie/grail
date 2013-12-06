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

(defun ver-ctl-log-dir ( path )
  (dir-path-if-accessible
    (expand-file-name (concat (ver-ctl-root path) "/logs/"))) )

(defun ver-ctl-log-files ()
  (let
    ((log-dir (ver-ctl-log-dir buffer-file-name)))

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

;; (ver-ctl-log-file-pairs (ver-ctl-log-files))

;; (ver-ctl-log-completion "foo" (ver-ctl-log-file-pairs (ver-ctl-log-files)) )

(defun ver-ctl-log-select ( prompt )
  (let
    ((existing-logs (ver-ctl-log-file-pairs (ver-ctl-log-files)) ))

    (if existing-logs
      (let
        ((log-pair (ver-ctl-log-completion prompt existing-logs)))

        (if log-pair
          (cdr log-pair)
          nil))
      (progn
        (message "no log files found! ")
        (throw 'no-log-files nil)) ) ))

(defun ver-ctl-log-file-open ()
  (interactive)
  (catch 'no-log-files
    (let
      ((log-file (ver-ctl-log-select "choose log: ")))

      (if log-file
        (switch-to-buffer
          (pop-to-buffer (find-file-noselect log-file) nil t))
        (message "no log selected!")) )) )

(defun ver-ctl-log-bindings ()
  (let
    ((ver-map (make-sparse-keymap)))

    (define-key ver-map "o" 'ver-ctl-log-file-open)
    (local-set-key (kbd "C-c l") ver-map)))

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
