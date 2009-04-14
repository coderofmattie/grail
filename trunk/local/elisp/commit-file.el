;;;----------------------------------------------------------------------
;; commit-file.el
;; written by: Mike Mattie
;; copyright: Mike Mattie 2009
;; license: GPL v3.
;;;----------------------------------------------------------------------

(require 'cm-path)
(require 'vc-logedit-hook)
(require 'vc-bzr)
(require 'bzr-ui)

(defcustom commit-file-name "commit-changelog.txt"
  "the filename of the commit changelog draft")

(defun commit-draft-dir ( file )
  "commit-draft-dir FILE

  return the commit-changelog directory for FILE.
  "
  (vc-bzr-root file))

(defun edit-commit-draft ()
  "edit-commit-draft

   Prompt for a branch. Hitting enter selects the branch that the
   current buffer is visiting. After a branch is selected the
   commit-file for that branch is opened.

   This scopes the commit-draft to the branch, however by supplying
   a sub-directory as the \"branch\" the user can make commit drafts
   in sub-directories.
  "
  (interactive)
  (let*
    ((branch  (bzr-prompt-for-branch "draft commit for branch"))
     (path    (concat branch commit-file-name)))

    (let
      ((draft-buffer (find-buffer-visiting path)))

      (if (bufferp draft-buffer)
        (switch-to-buffer draft-buffer)
        (progn
          (find-file path)
          (rename-buffer (concat (make-branch-path-repository-relative path)  ":changelog")) )) )))

(defun delete-commit-draft ()
  "delete-commit-changelog

   delete the commit changelog draft for the branch entered by
   the user. If there is a buffer it is killed and the file is
   deleted from disk.
  "
  (interactive)
  (let*
    ((branch  (bzr-prompt-for-branch "draft commit for branch"))
     (path    (concat branch commit-file-name))
     (buffer (find-buffer-visiting path)))

    (when buffer
      (with-current-buffer buffer (set-buffer-modified-p nil))
      (kill-buffer buffer))

    (delete-file path)))

;; (defun logedit-ping ()
;;  (message "vc-log-fileset is %s" (princ vc-log-fileset)))

(defun logedit-insert-changelog-draft ()
  (message "vc-log-fileset is %s" (princ vc-log-fileset))

  ;; clear the done list
  (setq commit-files-done nil)
  ;; create a list of the commit files that were used
  (set (make-local-variable 'commit-file-changelogs-used) nil)

  (mapc
    (lambda ( path )
      (let*
        ((draft-dir   (commit-draft-dir path))
         (commit-file (concat draft-dir commit-file-name)))

        ;; when the file is found insert the contents with a banner
        ;; showing the directory in which it was found.
        (when (and
                (not (member draft-dir commit-file-changelogs-used))
                (file-readable-p commit-file))

          ;; insert the contents of the commit draft with a header
          ;; showing which directory it came from.
          (insert (format "Changelog for %s\n" draft-dir))
          (insert-file-contents commit-file)

          ;; add this directory to the list so we don't process it again.
          (push draft-dir commit-file-changelogs-used)) ))

    vc-log-fileset))

(defun activate-commit-file ()
  "activate commit-file by hooking vc-new-logedit-hook"
  (interactive)
  (add-hook 'vc-new-logedit-hook 'logedit-insert-changelog-draft))

(provide 'commit-file)

