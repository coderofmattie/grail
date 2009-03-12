(quote-string-for-shell "http://www.dr-qubit.org/download.php?file=predictive/auto-overlays.tar.gz")

(grail-recursive-delete-directory (concat (getenv "HOME") "/" "foo"))

;; file OK
(grail-decompose-installer-type "file")

;; tar|compression ok
(grail-decompose-installer-type "tar:bz2")

;; error on bogus spec
(grail-decompose-installer-type "tarbz2")


;; okay too
(grail-define-installer "bob" "file" "foo.url")

;; okay
(grail-define-installer "bob" "tar:bz2" "bob.tar.bz2")

;; not ok, no url
(grail-define-installer "bob" "tar:bz2")

(grail-define-installer "icicles" "file"
  '("icicles.el"      . "http://www.emacswiki.org/cgi-bin/wiki/download/icicles.el")
  '("icicles-chg.el"  . "http://www.emacswiki.org/cgi-bin/wiki/download/icicles-chg.el")
  '("icicles-cmd.el"  . "http://www.emacswiki.org/cgi-bin/wiki/download/icicles-cmd.el")
  '("icicles-doc1.el" . "http://www.emacswiki.org/cgi-bin/wiki/download/icicles-doc1.el")
  '("icicles-doc2.el" . "http://www.emacswiki.org/cgi-bin/wiki/download/icicles-doc2.el")
  '("icicles-face.el" . "http://www.emacswiki.org/cgi-bin/wiki/download/icicles-face.el")
  '("icicles-fn.el"   . "http://www.emacswiki.org/cgi-bin/wiki/download/icicles-fn.el")
  '("icicles-mac.el"  . "http://www.emacswiki.org/cgi-bin/wiki/download/icicles-mac.el")
  '("icicles-mcmd.el" . "http://www.emacswiki.org/cgi-bin/wiki/download/icicles-mcmd.el")
  '("icicles-menu.el" . "http://www.emacswiki.org/cgi-bin/wiki/download/icicles-menu.el")
  '("icicles-mode.el" . "http://www.emacswiki.org/cgi-bin/wiki/download/icicles-mode.el")
  '("icicles-opt.el"  . "http://www.emacswiki.org/cgi-bin/wiki/download/icicles-opt.el")
  '("icicles-var.el"  . "http://www.emacswiki.org/cgi-bin/wiki/download/icicles-var.el")
  '("icomplete+.el"   . "http://www.emacswiki.org/cgi-bin/wiki/icomplete+.el/download/icomplete+.el")
  '("hexrgb.el"       . "http://www.emacswiki.org/cgi-bin/wiki/icomplete+.el/download/hexrgb.el"))

(defun grail-archive-installer ( name url type-and-compression )
  "grail-archive-installer"
  (message "would install %s from %s archive type %s" name url (princ type-and-compression)))


(defun grail-file-installer ( name url &optional path )
  (message "would install %s from %s to %s" name url
    (if path
      (format "grail-dist-dir + %s" path)
      "grail-dist-dir")))

(grail-run-installer
  (grail-define-installer "icicles" "file" "http://www.emacswiki.org/cgi-bin/wiki/download/icicles.el"))

(grail-run-installer
  (grail-define-installer "icicles" "file"
    '("icicles.el"      . "http://www.emacswiki.org/cgi-bin/wiki/download/icicles.el")
    '("icicles-chg.el"  . "http://www.emacswiki.org/cgi-bin/wiki/download/icicles-chg.el")))

(grail-run-installer
  (grail-define-installer "icicles" "tar:bz2" "http://www.emacswiki.org/cgi-bin/wiki/download/icicles.tar.bz2"))

(grail-run-installer
  (grail-define-installer "icicles" "tar:bz2"
    '("foo.tar" . "http://www.emacswiki.org/cgi-bin/wiki/download/icicles.tar.bz2")
    '("bar.tar" . "bar.url")))

(setq foo 'cfg-mod-icicle-install)

(put foo 'pkg-dir "icicles")

(grail-install-package "icicles" 'cfg-mod-icicle-install)

(grail-install-package "icicles" '(("foo" . "bar") ("baz" . "bing")))

(grail-repair-by-installing 'foo '(("foo" . "bar")))
(grail-repair-by-installing 'fubar '(("foo" . "bar") ("baz" . "bing")))

;;  ("baz" . "bing")
(grail-activate-with-recovery test foo
  (("foo" . "bar"))

  (setq foo "dead sexy")
  (message "I am %s" foo))

(type-of cfg-mod-icicle-install)

(defun retro-major-mode-hook ( mode-match hook hook-fn )

  ;; install the hook
  (add-hook hook hook-fn)

  (when grail-recovery-mode
  ;; retro the hook to existing buffers
  (mapc (lambda ( buf )
          (with-current-buffer buf
            (when (and (stringp major-mode) (string-match mode-match major-mode))
              (funcall hook-fn)) )
          ) (buffer-list)) ))

;; retro-install-hook

;; (buffer-list) ; get the buffer list

(add-hook 'emacs-lisp-mode-hook
      (lambda ()
        (show-paren-mode))

;; need to search for the buffer list and apply the hooks that are found.

(grail-in-load-path-p "mic-parens") ;; should fail
(grail-in-load-path-p "mic-paren")  ;; should succeed.

(message "test the grail diagnostics %s"
  (format-signal-trap (diagnostic-load-elisp (error "%s" "bad things happened"))))

(grail-repair-dependency-fn 'mic-paren "http://www.emacswiki.org/cgi-bin/emacs/download/mic-paren.el")

