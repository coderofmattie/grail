;;----------------------------------------------------------------------
;; mattie.el
;; Primary Author: Mike Mattie
;; Copyright: Mike Mattie (2007)
;; License: GPL v3.
;;----------------------------------------------------------------------

;; this function was created because file-readable-p is strangely
;; akward in that it returns t instead of the path it was given which
;; neccesitates this silly wrapper. Consider sending this upstream as
;; a patch or add-on to file-readable-p"

(defun file-if-readable ( file )
  "if file is a readable path return file or nil"
  (if (file-readable-p file)
    file))

(defun print-hex ( number )
  "print the hex of a number, faster than firing up calc mode"
  (message "the hex is %x" number))

;; this is somewhat silly now since modes turn it on.
(defun show-bad-ws()
  (interactive)
  (highlight-regexp "\t"))

(defun rid-window ()
  "get rid of the current window"
  (interactive)
  (delete-windows-on (current-buffer)))

(defun insert-key-notation ()
  "inject a complete \(kbd \"sequence\"\) with key notation for a key sequence given by prompt"
  (interactive)
  (insert "(kbd \"")
  (insert (format-kbd-macro (read-key-sequence "Key? " nil t)))
  (insert "\")"))

(defun visit-url ( url )
  "visit a url in a new buffer"
  (interactive "sURL? ")
  (progn
    (switch-to-buffer (generate-new-buffer url))
    (url-insert-file-contents url)))

(defun xml-before-doc-close ()
  "move the point immediately before the closing of the document"
  (interactive)
  (end-of-buffer)
  (re-search-backward "</" nil t))

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

;; required for my patched em-unix, note: merged upstream, may collide
;; on a update.
(defun nil-blank-string ( string )
  "if a string is all blanks return nil, if there are non-blank characters return the string"
  (if (string-match "[^[:blank:]]" string ) string))
