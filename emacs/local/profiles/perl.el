;;----------------------------------------------------------------------
;; perl5
;;----------------------------------------------------------------------

;; make cperl the default in all cases.
(mapc
  (lambda (pair)
    (if (eq (cdr pair) 'perl-mode)
      (setcdr pair 'cperl-mode)))
     (append auto-mode-alist interpreter-mode-alist))

(require 'cperl-mode)

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

(add-hook 'cperl-mode-hook
  (lambda ()
    (set-face-foreground cperl-pod-face "orange3")

    (configure-for-navigation 'forward-word 'backward-word)
    (configure-for-programming 'perl-list-fn-signatures "perl-mode")

    (local-set-key (kbd "C-h f") 'cperl-perldoc-at-point)

    (turn-on-dwim-tab 'cperl-indent-command)

    (procedural-smart-parens-editing)
    (setq sp-escape-char "\\"))
  t)
