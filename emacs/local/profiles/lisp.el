;;----------------------------------------------------------------------
;; lisp.el
;; Primary Author: Mike Mattie
;; Copyright: Mike Mattie 2009
;;----------------------------------------------------------------------

(defun lisp-smart-region-left ()
  (interactive)

  (set-mark-command)
  (sp-backward-up-sexp) )

(defun lisp-smart-region-right ()
  (interactive)

  (set-mark-command)
  (sp-up-sexp) )

(defun insert-smart-remapped-char ( character )
  (setq last-command-event character)
  (sp--self-insert-command 1) )

(defun insert-dumb-remapped-char ( character )
  (insert-char character)
  (self-insert-command 0) )

(defun lisp-smart-parens ()
  "bind the parentheses to the brace keys, while the shifted
   paren keys become the braces."
  (interactive)

  ;; make the parentheses a bit easier to type, less shifting.
  (local-set-key (kbd "[") (lambda () (interactive) (insert-smart-remapped-char ?\( )) )
  (local-set-key (kbd "]") (lambda () (interactive) (insert-dumb-remapped-char ?\) )) )

  (local-set-key (kbd "(") (lambda () (interactive) (insert-smart-remapped-char ?\[)) )
  (local-set-key (kbd ")") (lambda () (interactive) (insert-dumb-remapped-char ?\])) ) )

(defun lisp-smart-navigation ()
  (configure-for-navigation 'sp-next-sexp 'sp-previous-sexp)

  (local-set-key (kbd "<C-up>") 'sp-backward-up-sexp)
  (local-set-key (kbd "<C-down>") 'sp-up-sexp)

  (local-set-key (kbd "M-d") 'sp-kill-sexp)

  (local-set-key (kbd "<C-left>") 'lisp-smart-region-left) )

(defun lisp-smart-parens-editing ()
  (smartparens-mode 1)

  (lisp-smart-navigation)
  (lisp-smart-parens) )


(syntax-move/bind-next 'sp-forward-sexp)


(provide 'profile/lisp)
