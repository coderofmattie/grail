;;;----------------------------------------------------------------------
;; commit-file.el
;; written by: Mike Mattie
;; copyright: Mike Mattie 2009
;; license: GPL v3.
;;;----------------------------------------------------------------------

(require 'vc-logedit-hook)

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

   in the directory of the currently selected buffer edit the draft of the
   commit changelog.
  "
  (interactive)
  (find-file (commit-file-for buffer-file-name)) )

(defun delete-commit-draft ()
  "delete-commit-changelog

   in the directory of the currently selected buffer edit the changelog entry
   in progress file:commit
  "
  (interactive)
  (let*
    ((commit-file (commit-file-for (buffer-file-name)))
     (buffer (find-buffer-visiting commit-file)))

    (when buffer
      (with-current-buffer buffer (set-buffer-modified-p nil))
      (kill-buffer buffer))

    (delete-file commit-file)))

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
            (when (file-readable-p commit-file)
              (insert (format "Changelog for %s\n" dir))
              (insert-file commit-file)
              (push commit-file-changelogs-used commit-file))

            ;; add this directory to the list so we don't process it again.
            (push commit-files-done dir))) ))
    vc-log-fileset))

(add-hook 'vc-new-logedit-hook 'logedit-insert-commit-changelog-files)

(provide 'commit-file)

