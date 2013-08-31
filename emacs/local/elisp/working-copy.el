;;----------------------------------------------------------------------
;; working-copy.el
;; Primary Author: Mike Mattie
;; copyright: Mike Mattie 2009
;;----------------------------------------------------------------------

(eval-when-compile
  (require 'cl))

;; When programming it is often handy to merge by hunks to facilitate
;; clean commits, and keep the code working.

;; working-copy defines three principle commands: wc,cc,and wc-diff.

;; wc: open a working copy of a file which is the name of the file
;;     with the extension ".working-copy" added.

;; wc: open a checkout copy of a file which serves as the tunk.

;; wc-diff: start ediff between the working copy and the trunk.

(require 'cm-util)

(defun wc-set-auto-mode ( file )
  "run set-auto-mode with the original file-name visible so that the
   file extension based matching will work correctly"
  (let
    ((buffer-file-name file)) ;; set-auto-mode uses filename. shadow it.
    (set-auto-mode t)))

(defun load-or-copy-ancestor ( file extension type &optional fetch-copy )
  "load or copy the ancestor of FILE.

   A buffer is either found or created. If it is an empty file
   the ancestor of the file is inserted into the buffer.

   Checkout -> Working Copy

   The major mode is set using the original file-name so that the
   extensions to differentiate the copies on disk do not baffle
   set-auto-mode.

   The buffer is renamed to indicate that all the buffer
   preparation work has been completed.
  "
  (lexical-let*
    ((path    (expand-file-name
                (if extension
                  (concat file "." extension)
                  file)))
     (buffer  (or
                (find-buffer-visiting path)
                (save-excursion (find-file path))))

     (name    (concat (file-name-nondirectory file) "/" type)))

    (unless (string-equal (buffer-name buffer) name)
      (with-current-buffer buffer
        (when (buffer-empty-p)
          (when (functionp fetch-copy)
            (funcall fetch-copy buffer file)
            (write-file path)))

        (wc-set-auto-mode file)

        (set (make-local-variable 'wc-trunk) file)

        (rename-buffer name)))
    buffer))

(defun wc-trunk ()
  (if (boundp 'wc-trunk)
    wc-trunk
    (signal 'error (format "wc-trunk is not bound for %s" buffer-file-name))))


(defun wc ( file )
  "wc FILE

   Find the Working Copy of a source file in a existing buffer,
   or by loading the file from disk.

   The Working Copy is where new changes are made directly.
  "
  (interactive "fFind source Working Copy? ")

  (switch-to-buffer (wc-wc-buffer file)))

(defun wc-cc-buffer ( file )
  (load-or-copy-ancestor
    file
    nil
    "checkout"))

(defun wc-wc-buffer ( file )
  (load-or-copy-ancestor
    file
    "working-copy"
    "wc"
    (lambda ( buffer file )
      (with-current-buffer (wc-cc-buffer file)
        (copy-to-buffer buffer (point-min) (point-max)))) ))

(defun cc ( file )
  "Find the Checkout copy of a source file"
  (interactive "fFind source Checkout Copy? ")
  (switch-to-buffer (wc-cc-buffer file)))

(defun wc-diff ()
  "merc diff is a simple form of cherry picking that uses ediff"
  (interactive)
  (save-excursion
    (ediff-buffers (wc-cc-buffer (wc-trunk)) (current-buffer)) ))

(provide 'working-copy)
