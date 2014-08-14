


;====================== TODO ========================================

;;----------------------------------------------------------------------
;; installation support routines.
;;----------------------------------------------------------------------

(defvar grail-save-downloads nil
  "when t downloaded archive files will be saved to grail-dist-dir")

(defun grail-recursive-delete-directory ( path )
  "grail-recursive-delete-directory PATH

   recursively delete the directory PATH. t on success.
  "
  (grail-fail
    "grail-recursive-delete-directory"
    "deleting a directory"

    (unless (equal 0 (call-process-shell-command "rm" nil nil nil "-r" path))
      (grail-signal-fail "grail-recursive-delete-directory"
        (format "path %s is not a directory or the user does not have permissions" path)) ) ))

(defun grail-download-dir-and-file-path ( name )
  (let
    ((dl-dir  nil))

    (when (condition-case trapped-error
          (progn
            (setq dl-dir (if grail-save-downloads
                            grail-dist-dir
                            (make-temp-file "grail" t)))
            t)
          (error
              (throw 'grail-trap
                '((message "grail: grail-download-dir-and-file-path could not create a download path for %s" name))) ))
      (cons dl-dir (expand-file-name (concat dl-dir "/" name))) )))

(defun grail-cleanup-download ( dl-dir-and-file &optional ignore-save )
  "grail-cleanup-download

   delete the directory and the downloaded files.

   TODO: save downloads option.
  "
  (when dl-dir-and-file
    (if grail-save-downloads
      ;; when grail-save-downloads is enabled absolutely do not recursive delete !
      (when (not ignore-save)
        (delete-file (cdr dl-dir-and-file)))
      ;; otherwise it is a temp dir so nuke it
      (grail-recursive-delete-directory (car dl-dir-and-file))) ))

;;----------------------------------------------------------------------
;; async chain.
;;----------------------------------------------------------------------

(defun grail-buffer-exists-p ( buffer-name )
  (if (not (eq nil (get-buffer buffer-name)))
    t
    nil))

(defun get-time-in-seconds ()
  (truncate (time-to-seconds (current-time))))

(defun grail-process-async-wait ( proc-buffer-name )
  (let
    ((timeout 30)
     (last-size (buffer-size (get-buffer proc-buffer-name)))
     (last-time (get-time-in-seconds)))

  (catch 'timeout
    (while (grail-buffer-exists-p proc-buffer-name)
      (sleep-for 0.25)

      (let
        ((check-size (buffer-size (get-buffer proc-buffer-name))))

        (if (> check-size last-size)
          (progn
            (setq last-size check-size)
            (setq last-time (get-time-in-seconds)))
          (when (>  (- (get-time-in-seconds) last-time) timeout)
            (throw 'timeout nil)) )))
    t)))


;;----------------------------------------------------------------------
;; grail-run-and-wait
;;----------------------------------------------------------------------

(defun grail-run-and-wait ( base-name fn )
  (let*
    ((output-buffer-name   (generate-new-buffer-name base-name))
     (output-buffer-object (pop-to-buffer output-buffer-name nil t))
     (status nil))

    (funcall fn output-buffer-object)

    (grail-process-async-wait output-buffer-name) ))

;;----------------------------------------------------------------------
;; installation library
;;----------------------------------------------------------------------

(defun grail-wget-url-async ( url path output-buffer )
  "grail-wget-url-async URL PATH OUTPUT-BUFFER

   retrieve the URL to PATH, with OUTPUT-BUFFER as the output
   buffer. The process object created is returned, or nil if a
   process could not be created.
  "
  (condition-case trapped-error
    (start-process-shell-command "grail-wget" output-buffer
      "wget"
      "--progress=dot:binary"
      (shell-quote-argument url) "-O" (shell-quote-argument path))
    (error
      (progn
        (message "grail-wget-url failed %s" (format-signal-trap trapped-error))
        nil)) ))


;;
;; tar
;;

(defvar grail-untar-strip-command nil)

(defun grail-untar-strip-by-el ()
  (setq grail-untar-strip-command (concat "--wildcards" " " (shell-quote-argument "*.el"))) )

(defun grail-untar-strip-by-depth ( levels )
  (setq grail-untar-strip-command
    (if (> levels 0)
      (concat "--strip-components" " " (number-to-string levels))
      " ")))

(defun grail-untar-async ( path target-dir compression output-buffer )
  "grail-untar-async PATH DIR COMPRESSION OUTPUT-BUFFER

   untar PATH in DIR with output going to OUTPUT-BUFFER.
   return the process object or nil if there was an error.
  "
  (condition-case trapped-error
    (start-process-shell-command "grail-untar" output-buffer
      "tar"
      (concat
        "xv"
        (cond
          ((equal "gz"  compression) "z")
          ((equal "bz2" compression) "j")
          (t (signal error (format "grail: error! unsupported compression %s" compression))))
        "f")
      (shell-quote-argument path)
      "-C" (shell-quote-argument target-dir)
      grail-untar-strip-command)
    (error
      (progn
        (message "grail-untar-async failed %s" (format-signal-trap trapped-error))
        nil)) ))

(defun grail-untar-local-archive ( path compression tar-buffer )
  "grail-untar-local-archive PATH COMPRESSION

   extract the local archive PATH in directory name with COMPRESSION.
  "
  (lexical-let
    ((archive-path   path)
     (grail-buffer   tar-buffer))

    (async-exec-chain
      ;; start the untar
      (lambda ()
        (grail-untar-async archive-path grail-dist-elisp compression grail-buffer))

      ;; if it doesn't start
      (lambda ()
        (message "archive program did not start for %s!" archive-path))

      ;; FIXME: how do we clean up the target directory ?
      (lambda ( exit-status )
        (message "extracting %s failed! status %s " archive-path exit-status))

      ;; what to do when it finishes.
      (lambda ()
        (message "extracting %s has completed." archive-path))

      ;; no chaining
      nil) ))

(defun grail-untar-remote-archive ( name url compression tar-buffer)
  "grail-untar-remote-archive NAME URL COMPRESSION

   Download a tarball from a remote url and install it. It is currently
   hard-coded for tar, but that could be changed fairly easily.
  "
  (save-excursion
    (lexical-let*
      ((target-dir      name)
       (dl-dir-and-file nil)
       (old-window      (selected-window))
       (install-buffer  tar-buffer))

      (catch 'abort
        ;; confirm with the user that they want to install the file.
        (unless (yes-or-no-p (format "download and install %s? " name))
          (throw 'abort nil))

        ;; signal the start of the download in the grail buffer.
        (insert (format "Starting the download of %s\n" url))

        ;; create a temporary directory to download into
        (unless (setq dl-dir-and-file (grail-download-dir-and-file-path (concat name ".tar." compression)))
          (throw 'abort "could not create a temporary directory for the download"))

        (lexical-let
          ((dl-url  url)
           (compression-type compression))

          (async-exec-chain
            ;; start the download with wget
            (lambda ()
              (grail-wget-url-async
                dl-url
                (cdr dl-dir-and-file)
                install-buffer))

            ;; the downloader doesn't start cleanup function
            (lambda ()
              (insert "could not start the download! Install aborted.\n")
              (grail-cleanup-download dl-dir-and-file t))

            ;; the downloader fail cleanup function
            (lambda ( exit-status )
              (grail-cleanup-download dl-dir-and-file t)
              (message "download of %s failed! Install aborted, and downloads deleted." (cdr dl-dir-and-file)))

            ;; the downloader succeeded function
            (lambda ()
              (insert "grail: download completed\n")
              t)

            ;; the chain function
            (lambda ()
              (async-exec-chain
                ;; start the untar
                (lambda ()
                  (message "starting the untar")
                  (grail-untar-async (cdr dl-dir-and-file) (grail-dist-install-directory target-dir)
                    compression-type install-buffer))

                ;; tar doesn't start cleanup function
                (lambda ()
                  (insert "could not start tar to extract the downloaded archive. Install aborted, deleting downgrail: cleaning up downloads
loads.\n")
                  (grail-cleanup-download dl-dir-and-file t))

                ;; the tar fail cleanup function
                (lambda ( exit-status )
                  (insert (format "could not install files in %s from downloaded archive." grail-dist-elisp))
                  (grail-cleanup-download dl-dir-and-file t))

                ;; the tar succeeded function
                (lambda ()
                  (insert "grail: cleaning up downloads\n")
                  (grail-cleanup-download dl-dir-and-file)
                  (kill-buffer install-buffer)
                  t)

                ;; terminate the chain.
                nil))) )
        ;; return nil if an abort is not thrown.
        nil)) ))

;;----------------------------------------------------------------------
;; tar installer
;;
;;----------------------------------------------------------------------

(defvar grail-tar-buffer "*grail tar*")

(defun grail-tar-local-installer ( name url compression )
  (let
    ((name-arg        name)
     (url-arg         url)
     (compression-arg compression))

    (grail-run-and-wait grail-tar-buffer
      (lambda ( run-buffer )
        (grail-untar-remote-archive name-arg url-arg compression run-buffer))) ))

(defun grail-tar-remote-installer ( name url compression )
  (let
    ((name-arg        name)
     (url-arg         url)
     (compression-arg compression))

    (grail-run-and-wait grail-tar-buffer
      (lambda ( run-buffer )
        (grail-untar-remote-archive name-arg url-arg compression run-buffer))) ))

(defun grail-tar-builder ( url target compression )
  ;; When installing a local archive only the path and the compression
  ;; need be known, as the target directory and the like cannot be
  ;; ascertained without inspecting the archive.

  ;; for a remote archive pass the name, the url, and the
  ;; compression. The name is used for naming the download. This is
  ;; especially useful when the downloads are saved.
  (if (string-match "archived:\\(.*\\)" url)
    `(grail-tar-local-installer  ,(concat grail-dist-archive (match-string 1 url)) ,compression)
    `(grail-tar-remote-installer ,target ,url ,compression)))

;====================== TODO ========================================
