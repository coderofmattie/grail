;;----------------------------------------------------------------------
;; complete.el
;;----------------------------------------------------------------------

;; icicles is not just tab completion. It is a powerful experiment
;; in solving some fundamental problems in computer human interaction.
;; This rabbit hole is worthy of a book.

(defvar grail-group-icicles-installer
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
  "the icicles installer specification")

(grail-activate-with-recovery "complete" icicles grail-group-icicles-installer
  (setq
    icicle-generic-S-tab-keys (cons (kbd "<C-tab>") nil)   ;; I use S-tab already
    icicle-customize-save-flag nil                         ;; disable auto-save of customize
    )

  (icy-mode))
