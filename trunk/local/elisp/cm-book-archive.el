;;;----------------------------------------------------------------------
;; cm-book-archive.el
;; written by Mike Mattie
;;;----------------------------------------------------------------------

;; not a typical elisp library, it is a index of my digitized book archive.

;; I was looking for a way to come up with a decent interface for browsing
;; my archive. The easiest solution would be to use the existing command
;; completion mechanism rather than my own.

;; Then I define a tree scheme in the naming of the commands and I have
;; browsing with completion of the archive.

(defvar mattie-archive-path
  (expand-file-name (concat (getenv "HOME") "/archive/")))

(defun mattie-archive-url ( book-path )
  (concat "file://" mattie-archive-path book-path))

(defun book:how-to-design-programs ()
  (interactive)
  (funcall browse-url-browser-function (mattie-archive-url "programming/scheme/htdp/index.html")))

(provide 'cm-book-archive)
