;;;----------------------------------------------------------------------
;; commit-file.el
;; written by: Mike Mattie
;; copyright: Mike Mattie 2009
;; license: GPL v3.
;;;----------------------------------------------------------------------

(require 'vc-logedit-hook)
(require 'rc-navigate)

(defvar commit-file-name "commit-changelog.txt"
  "the filename of the commit changelog draft")

(defun commit-file-for ( file )
  "commit-file-for FILE

  return the commit-changelog path for FILE, which is the directory
  of FILE, plus the value of commit-file-name.
  "
  (concat (file-name-directory file) commit-file-name))

(defun edit-commit-draft ()
  "edit-commit-draft

   Prompt for a branch. Hitting enter selects the branch that the
   current buffer is visiting. After a branch is selected the
   commit-file for that branch is opened.
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

(defun logedit-ping ()
  (message "vc-log-fileset is %s" (princ vc-log-fileset)))

(defvar commit-files-done nil
  "a list of the directories where commit-file has already searched
   for a commit-changelog file to prevent duplication with filesets")

(defun logedit-insert-commit-changelog-files ()
  (message "vc-log-fileset is %s" (princ vc-log-fileset))

  ;; clear the done list
  (setq commit-files-done nil)
  ;; create a list of the commit files that were used
  (set (make-local-variable 'commit-file-changelogs-used) nil)

  (mapc
    (lambda ( path )
      (let
        ((dir (file-name-directory path)))

        (unless (member dir commit-files-done)
          (let
            ((commit-file (concat dir commit-file-name)))

            ;; when the file is found insert the contents with a banner
            ;; showing the directory in which it was found.
            (when (and (file-readable-p commit-file) (not (member commit-file-changelogs-used)))
              (insert (format "Changelog for %s\n" dir))
              (insert-file commit-file)
              (push commit-file-changelogs-used commit-file))

            ;; add this directory to the list so we don't process it again.
            (push commit-files-done dir))) ))
    vc-log-fileset))

(add-hook 'vc-new-logedit-hook 'logedit-insert-commit-changelog-files)

(provide 'commit-file)

