;;----------------------------------------------------------------------
;; complete.el
;;----------------------------------------------------------------------

;; icicles is not just tab completion. It is a powerful experiment
;; in solving some fundamental problems in computer human interaction.
;; This rabbit hole is worthy of a book.

;; the installer for icicles.
(grail-define-installer
  'cfg-mod-icicle-install
  '(("icicles"      . "http://www.emacswiki.org/cgi-bin/wiki/download/icicles.el")
    ("icicles-chg"  . "http://www.emacswiki.org/cgi-bin/wiki/download/icicles-chg.el")
    ("icicles-cmd"  . "http://www.emacswiki.org/cgi-bin/wiki/download/icicles-cmd.el")
    ("icicles-doc1" . "http://www.emacswiki.org/cgi-bin/wiki/download/icicles-doc1.el")
    ("icicles-doc2" . "http://www.emacswiki.org/cgi-bin/wiki/download/icicles-doc2.el")
    ("icicles-face" . "http://www.emacswiki.org/cgi-bin/wiki/download/icicles-face.el")
    ("icicles-fn"   . "http://www.emacswiki.org/cgi-bin/wiki/download/icicles-fn.el")
    ("icicles-mac"  . "http://www.emacswiki.org/cgi-bin/wiki/download/icicles-mac.el")
    ("icicles-mcmd" . "http://www.emacswiki.org/cgi-bin/wiki/download/icicles-mcmd.el")
    ("icicles-menu" . "http://www.emacswiki.org/cgi-bin/wiki/download/icicles-menu.el")
    ("icicles-mode" . "http://www.emacswiki.org/cgi-bin/wiki/download/icicles-mode.el")
    ("icicles-opt"  . "http://www.emacswiki.org/cgi-bin/wiki/download/icicles-opt.el")
    ("icicles-var"  . "http://www.emacswiki.org/cgi-bin/wiki/download/icicles-var.el")
    ("icomplete+"   . "http://www.emacswiki.org/cgi-bin/wiki/icomplete+.el/download/icomplete+.el")
    ("hexrgb"       . "http://www.emacswiki.org/cgi-bin/wiki/icomplete+.el/download/hexrgb.el"))
  "icicles")

(grail-activate-with-recovery "complete" icicles
  cfg-mod-icicle-install

  (setq
    icicle-generic-S-tab-keys (cons (kbd "<C-tab>") nil)   ;; I use S-tab already
    icicle-customize-save-flag nil                         ;; disable auto-save of customize
    )

  (icy-mode))
