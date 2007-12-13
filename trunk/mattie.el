;;----------------------------------------------------------------------
;; mattie.el
;; Primary Author: Mike Mattie
;; Copyright: Mike Mattie (2007)
;; License: GPL v3.
;;----------------------------------------------------------------------

(defun map-filter-nil ( func &rest seq )
  "map-filter-nil. apply the function to the arguements ala mapcar.
   Filter any nil elements of the sequence before the function is
   applied, and after the function is applied."

  (if (car seq)
    (let
      ((result (funcall func (car seq))))
      (if result
        (cons result (apply 'map-filter-nil func (cdr seq)))
        (apply 'map-filter-nil func (cdr seq))
        ))
    (if (cdr seq)
      (apply 'map-filter-nil func (cdr seq)))
    ))

(defun file-if-readable ( file )
  "this function was created because file-readable-p is strangely akward in that it returns t
   instead of the path it was given which neccesitates this silly wrapper. Consider sending
   this upstream as a patch or add-on to file-readable-p"

  (if (file-readable-p file)
    file))


(defun show-bad-ws()
  (interactive)
  (highlight-regexp "\t"))

(defun rid-window ()
  "get rid of the current window"
  (interactive)
  (delete-windows-on (current-buffer)))

(defun insert-key-notation ()
  "inject a complete (kbd "sequence") with key notation for a key sequence given by prompt"
  (interactive)
  (insert "(kbd \"")
  (insert (format-kbd-macro (read-key-sequence "Key? " nil t)))
  (insert "\")")
  )

(defun visit-url ( url )
  "visit a url in a new buffer"
  (interactive "sURL? ")
  (progn
    (switch-to-buffer (generate-new-buffer url))
    (url-insert-file-contents url)))

;;----------------------------------------------------------------------
;; fragile regex tricks
;;----------------------------------------------------------------------

;; BUG: this doesn't handle quoted delimiters, which should not be that hard.
(defun bounds-scan ( seek-bounds open-bound-p close-bound-p restart-position position level )
  "scan for the delimitation of a region. This is a general form of a
   simple algorithm that counts opening and closing delimiters to scan
   past nested delimited spans."
  (progn
    (goto-char position) ;; move to the starting position before scanning.
    (funcall seek-bounds)

    (cond
      ((funcall open-bound-p)
        (bounds-scan seek-bounds open-bound-p close-bound-p restart-position
          (funcall restart-position) (+ level 1)))

      ((funcall close-bound-p)
        (if (> level 0)
          ;; when we have a positive level to start with
          ;; scan again with a decremented level.
          (bounds-scan seek-bounds open-bound-p close-bound-p restart-position
            (funcall restart-position) (- level 1))

          ;; return point as we are done
          (point)
          ))
      ;; error ?
      )))

(defun bounds-scan-forward ( delimiters position )
  "entry point for bounds-scan forward. given delimiters: a
   string containing a pair of delimiting characters, which must
   be in \"open close\" order, scan forward for the bounding
   delimiter returning the position before the delimiter"

  (lexical-let
    ((open-delimiter (aref delimiters 0))
     (close-delimiter (aref delimiters 1)))

    (let
      ((close-at (bounds-scan
                   (lambda ()
                     (skip-chars-forward (concat "^" delimiters)))

                   (lambda ()
                     (char-equal open-delimiter (char-after)))

                   (lambda ()
                     (char-equal close-delimiter (char-after)))

                   (lambda ()
                     (+ (point) 1))

                   position 0)))
      ;; Keep the returned position within the delimiters if the
      ;; scan moved beyond the starting position.
      (if (> close-at position)
        (- close-at 1)
        position)
      )))

(defun bounds-scan-backward ( delimiters position )
  "entry point for bounds-scan backward. given delimiters: a
   string containing a pair of delimiting characters, which must
   be in \"open close\" order, scan backward for the bounding
   delimiter returning the position after the delimiter"

  (lexical-let
    ;; note the inversion of the order since we are looking backwards
    ((open-delimiter (aref delimiters 1))
     (close-delimiter (aref delimiters 0)))

    (let
      ((open-at (bounds-scan
                  (lambda ()
                    (skip-chars-backward (concat "^" delimiters)))

                  (lambda ()
                    (char-equal open-delimiter (char-before)))

                  (lambda ()
                    (char-equal close-delimiter (char-before)))

                  (lambda ()
                    (- (point) 1))

                  position 0)))
      ;; Keep the returned position within the delimiters if the
      ;; scan moved beyond the starting position.
      (if (< open-at position)
        (+ open-at 1)
        position)
      )))

(defun scan-lisp-list-close ()
  "wrapper for bounds-scan that searches for the closing delimiter of a lisp list"
  (bounds-scan-forward "()" (point)))

(defun scan-lisp-list-open ()
  "wrapper for bounds-scan that searches for the opening delimiter of a lisp list"
  (bounds-scan-backward "()" (point)))

(defun lisp-list-delete-body ()
  "delete the body of a lisp list including any nested lists"
  (interactive)
  (let
    ((open-pos (scan-lisp-list-open))
     (close-pos (scan-lisp-list-close)))

    (delete-backward-char (- close-pos open-pos))))

(defun xml-before-doc-close ()
  "move the point immediately before the closing of the document"
  (interactive)
  (end-of-buffer)
  (re-search-backward "</"))

;;----------------------------------------------------------------------
;; repl
;;
;; Handy tool for exploratory programming. pops a new frame with the
;; interpreter for the language running in a REPL loop.
;;----------------------------------------------------------------------

(defun repl ( lang )
  "start a REPL interpreter interface in a new frame based upon a
  given or inferred language parameter"

  (interactive "MLanguage: ")

  (lexical-let
    ((repl-frame (make-frame))
      )

    (select-frame-set-input-focus repl-frame)

    (cond
      ((string-equal "perl5" lang)
	(switch-to-buffer (make-comint "perl5 REPL" "/usr/bin/perl" nil "-d" "-e shell")))
      ((string-equal "elisp" lang)
        (ielm))
      (else
        (message "I don't support language %s" lang)))

  (add-hook 'kill-buffer-hook
    (lambda ()
      (delete-frame repl-frame))
    t t)
    ))

(defun examine-library (library-name)
  "examine the source of a library. Type the library name without
   any extension. If the library exists the source will be
   loaded"

  (interactive "F")
  (find-file-read-only (locate-library (concat library-name ".el")))
  )

;; This is a handy little function that allows you to localize
;; a distributed elisp source file. It assumes that the current
;; buffer is a distributed elisp file, and that localized-source-dir
;; points to a real directory.

(defun localize-distrib ()
  "localize a distributed lisp file by writing a copy of the file
   to a directory searched before the distributed lisp files"
  (interactive)

  (let
    ((new-name (file-name-nondirectory (buffer-file-name))))

    (let
      ((new-path
        (concat localized-source-dir
          (if (string-equal "gz" (file-name-extension new-name))
            (file-name-sans-extension new-name)
            (new-name)))))
      (if (yes-or-no-p (concat "localize distributed file " new-name " to " new-path))
        (write-file new-path)
        (message "aborted localizing distributed file"))
    )))

;; required for my patched em-unix, note: merged upstream, may collide
;; on a update.
(defun nil-blank-string ( string )
  "if a string is all blanks return nil, if there are non-blank characters return the string"
  (if (string-match "[^[:blank:]]" string ) string))

;;----------------------------------------------------------------------
;; local function library.
;;----------------------------------------------------------------------

;; taken from the Lisp Intro text as rendered by:
;; http://www.rattlesnake.com/intro/print_002delements_002dof_002dlist.html

;; modified slightly to issue debugging bread-crumbs to the Messages buffer
(defun debug-elements-of-list (list)
  "Print each element of LIST on a line of its own."
  (while list
    (message "debug: element %s" (car list))
    (setq list (cdr list))))

(defun merge-changes ()
  "Merge latest changes against the last checkout."
  (interactive)
  (let
    ( (merge-file (buffer-file-name))
      (wc-file (concat (buffer-file-name) ".merge"))
      )

    (save-excursion
      (let
        ((wc-buffer (progn
                      (write-file wc-file)
                      (buffer-name)))

          ;; using vc-workfile-version is necessary so that subsequent merges
          ;; get the correct head-buffer
          (head-buffer (vc-find-version merge-file (vc-workfile-version merge-file)))

          (merge-buffer (progn
                          (vc-revert-file merge-file)
                          (find-file merge-file)))
        )

        ;; ? check for an exit status from ediff
        (ediff-merge-buffers head-buffer wc-buffer nil nil merge-file)

        ;; ? make sure the changes were saved
        ))
      ))

