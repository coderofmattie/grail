;; don't use. fucking hard to figure out how to disable templates
;; in pde or harder than it should be.


(grail-load-package 'pde "git" "https://github.com/wenbinye/emacs-pde.git")

;; be careful about pde-load which is the usual way to load. triggers
;; transient mark amongst other things.
(strip-minor-mode-keymap 'pde-mode)

(require 'inf-perl)

(defconst profile/perl5-repl-name (borg-repl/repl-name profile/perl5-mode-name))

;; tried perlconsole because it was readline.

(setq pde-perl-program "perlconsole")

(defun profile/perl5-repl-setup ()

  ;; no actual send buffer so use send
  ;; region as a dummy function.
  (borg-repl/bind-repl profile/perl5-repl-name
    'run-perl
    'inf-perl-send
    'inf-perl-send-region
    'inf-perl-send-region) )

(add-hook 'cperl-mode-hook 'profile/perl5-repl-setup t)

(provide 'profile/perl5)

