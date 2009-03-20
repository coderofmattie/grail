;;----------------------------------------------------------------------
;; linux.el
;;----------------------------------------------------------------------

;; Gentoo has a file that tunes emacs and loads the third party
;; components managed by the package manager.

;; (load-elisp-if-exists "/usr/share/emacs/site-lisp/site-gentoo.el")

;;----------------------------------------------------------------------
;; sawfish.
;;----------------------------------------------------------------------
(eval-after-load 'sawfish
  '(add-hook 'sawfish-mode-hook
     (lambda ()
       ;; eval-defun will "reset" these forms as well as not echoing into the buffer.
       ;; this function/keybinding should be used exclusively to avoid frustrating
       ;; errors.
       (local-set-key (kbd "C-x e") 'sawfish-eval-defun)

       (dwim-tab-localize 'sawfish-complete-symbol) )))


