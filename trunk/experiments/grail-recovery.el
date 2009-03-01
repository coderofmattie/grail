


;; (defun reto-hook-mode ( mode hook-fn )

(mapc

(get-url-with-last-modified
  ;; can we build an update system in as well ?

  )


(defvar cfg-mod-icicle-install
  '(("icicles.el"      . "http://www.emacswiki.org/cgi-bin/wiki/download/icicles.el")
    ("icicles-chg.el"  . "http://www.emacswiki.org/cgi-bin/wiki/download/icicles-chg.el")
    ("icicles-cmd.el"  . "http://www.emacswiki.org/cgi-bin/wiki/download/icicles-cmd.el")
    ("icicles-doc1.el" . "http://www.emacswiki.org/cgi-bin/wiki/download/icicles-doc1.el")
    ("icicles-doc2.el" . "http://www.emacswiki.org/cgi-bin/wiki/download/icicles-doc2.el")
    ("icicles-face.el" . "http://www.emacswiki.org/cgi-bin/wiki/download/icicles-face.el")
    ("icicles-fn.el"   . "http://www.emacswiki.org/cgi-bin/wiki/download/icicles-fn.el")
    ("icicles-mac.el"  . "http://www.emacswiki.org/cgi-bin/wiki/download/icicles-mac.el")
    ("icicles-mcmd.el" . "http://www.emacswiki.org/cgi-bin/wiki/download/icicles-mcmd.el")
    ("icicles-menu.el" . "http://www.emacswiki.org/cgi-bin/wiki/download/icicles-menu.el")
    ("icicles-mode.el" . "http://www.emacswiki.org/cgi-bin/wiki/download/icicles-mode.el")
    ("icicles-opt.el"  . "http://www.emacswiki.org/cgi-bin/wiki/download/icicles-opt.el")
    ("icicles-var.el"  . "http://www.emacswiki.org/cgi-bin/wiki/download/icicles-var.el")
    ("icomplete+.el"   . "http://www.emacswiki.org/cgi-bin/wiki/icomplete+.el/download/icomplete+.el")
    ("hexrgb.el"       . "http://www.emacswiki.org/cgi-bin/wiki/icomplete+.el/download/hexrgb.el"))

  "The icicles install list.")

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

