;;----------------------------------------------------------------------
;; perl5
;;----------------------------------------------------------------------
(require 'cperl-mode)

(require 'dwim-tab)
(require 'mode-tools)
(require 'user-programming)

(remap-assoc-mode-to 'perl-mode 'cperl-mode)

(progn
  (setq
    cperl-indent-parens-as-block t

    cperl-indent-level 2
    cperl-continued-statement-offset 2

    cperl-close-paren-offset -2

    cperl-indent-subs-specially nil

    cperl-highlight-variables-indiscriminately t

    cperl-electric-parens nil))

(defconst perl-function-regex "sub")

(defun perl-list-fn-signatures ()
  (interactive)
  (occur perl-function-regex))

(defun profile/cperl-mode-setup ()
  (set-face-foreground cperl-pod-face "orange3")

  (configure-for-programming 'perl-list-fn-signatures "perl-mode")

  (local-set-key (kbd "C-h f") 'cperl-perldoc-at-point)

  (turn-on-dwim-tab)

  (grail-requires profile/syntax-tools "perl profile" "smart syntax"
    (profile/syntax-tools-setup) ) )

(add-hook 'cperl-mode-hook 'profile/cperl-mode-setup t)

(provide 'profile/perl)

