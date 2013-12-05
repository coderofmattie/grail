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

(defun ver-ctl-branch-list ()
  (let*
    ((vc-fileset (vc-deduce-fileset t))
     (backend (car vc-fileset)))

    (funcall (vc-find-backend-function backend 'branches)) ))

(defun ver-ctl-branch-current ()
  (car (ver-ctl-branch-list)))

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

    (define-key ver-map "s" 'ver-ctl-status)
    (define-key ver-map "x" 'ver-ctl-interface)
    (define-key ver-map "r" 'ver-ctl-status)

    (local-set-key (kbd "C-c v") ver-map)))

(defun ver-ctl-hook ()
  (make-variable-buffer-local 'mattie-modeline-branch)
  (setq mattie-modeline-branch 'ver-ctl-branch-current)

  (ver-ctl-bindings))

(add-hook 'configure-programming-hook 'ver-ctl-hook t)
