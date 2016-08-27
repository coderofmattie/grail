;;----------------------------------------------------------------------
;; syntax-tools
;;----------------------------------------------------------------------
(require 'mode-tools)
(require 'syntax-move)

(grail-load-package 'dash "git" "https://github.com/magnars/dash.el.git")

(grail-load-package 'smartparens "git" "https://github.com/Fuco1/smartparens.git")

;;
;; adapt smartparens
;;

(strip-minor-mode-keymap 'smartparens-mode)

(set-face-background 'sp-pair-overlay-face "grey10")
(set-face-background 'sp-wrap-overlay-face "grey10")
(set-face-background 'sp-wrap-tag-overlay-face "grey10")

(setq sp-navigate-reindent-after-up nil)

;;
;; create adapter to remap parens with smartparens in the mix
;;

;; this insert function in smartparen is gone now.
;; (defun insert-smart-remapped-char ( character )
;;   (setq last-command-event character)
;;   (sp--self-insert-command 1) )

(defun insert-smart-remapped-char ( character )
  (setq last-command-event character)
  (insert-char character) )

(defun insert-dumb-remapped-char ( character )
  (insert-char character)
  (self-insert-command 0) )

(defun profile/syntax-tools-lisp ()
  "bind the parentheses to the brace keys, while the shifted
   paren keys become the braces."
  (interactive)

  ;;
  ;; swap parens keys
  ;;

  (local-set-key (kbd "[") (lambda () (interactive) (insert-smart-remapped-char ?\( )) )
  (local-set-key (kbd "]") (lambda () (interactive) (insert-dumb-remapped-char ?\) )) )

  (local-set-key (kbd "(") (lambda () (interactive) (insert-smart-remapped-char ?\[)) )
  (local-set-key (kbd ")") (lambda () (interactive) (insert-dumb-remapped-char ?\])) )

  ;;
  ;; use syntatic move bindings for lisp
  ;;

  (syntax-move/bind-prev       'sp-next-sexp)
  (syntax-move/bind-next       'sp-prev-sexp)
  (syntax-move/bind-opening    'sp-beginning-of-sexp)
  (syntax-move/bind-closing    'sp-end-of-sexp)
  (syntax-move/bind-up-scope   'sp-up-sexp)
  (syntax-move/bind-down-scope 'sp-down-sexp)
  (syntax-move/bind-kill-expr  'sp-sexp)

  (syntax-move/local-keybindings) )

(defun profile/syntax-tools-mode-setup ()
  (interactive)
  (smartparens-mode 1) )

(provide 'profile/syntax-tools)
