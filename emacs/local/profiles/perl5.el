;;----------------------------------------------------------------------
;; perl5
;;----------------------------------------------------------------------
(require 'cperl-mode)

(require 'dwim-tab)
(require 'mode-tools)
(require 'user-programming)

(remap-assoc-mode-to 'perl-mode 'cperl-mode)

(defconst profile/perl5-mode-name "perl5")

(setq
  cperl-indent-parens-as-block t

  cperl-indent-level 2
  cperl-continued-statement-offset 2

  cperl-close-paren-offset -2

  cperl-indent-subs-specially nil

  cperl-highlight-variables-indiscriminately t

  cperl-electric-parens nil)

(defconst profile/perl5-function-regex "sub")

(defun profile/perl5-fn-signatures ()
  (interactive)
  (occur perl-function-regex))

(defun profile/perl5-mode-setup ()
  (set-face-foreground cperl-pod-face "orange3")

  (configure-for-programming 'profile/perl5-fn-signatures profile/perl5-mode-name)

  (buffer-ring/add profile/perl5-mode-name)
  (buffer-ring/local-keybindings)

  (turn-on-dwim-tab)

  (grail-require profile/syntax-tools "perl profile" "smart syntax"
    (profile/syntax-tools-mode-setup) ) )

(add-hook 'cperl-mode-hook 'profile/perl5-mode-setup t)

(grail-load-package 'pde "git" "https://github.com/wenbinye/emacs-pde.git")

(require 'pde-load)

(provide 'profile/perl5)

